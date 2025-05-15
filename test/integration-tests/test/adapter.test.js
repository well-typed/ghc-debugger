import { DebugClient } from '@vscode/debugadapter-testsupport';
import * as cp from 'child_process';
import * as net from 'net';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { mkdtempSync, cpSync } from 'node:fs';
import assert from 'assert';

function getFreePort() {
	return new Promise((resolve, reject) => {
		const server = net.createServer();
		server.listen(0, () => {
			const address = server.address();
			if (typeof address === 'object' && address?.port) {
				const port = address.port;
				server.close(() => resolve(port));
			} else {
				reject(new Error('Could not get free port'));
			}
		});
		server.on('error', reject);
	});
}

var dc;
let debuggerProcess;

describe("Debug Adapter Tests", function () {
    this.timeout(15000); // 15s
    const cwd = process.cwd();

    beforeEach( () => getFreePort().then(port => {

        debuggerProcess = cp.spawn('ghc-debug-adapter', ['--port', port.toString()]);

        const ready = new Promise((resolve, reject) => {
            const timeout = setTimeout(() => {
                reject(new Error("ghc-debugger did not signal readiness in time"));
            }, 15000); // 15 second timeout

            debuggerProcess.stdout.on('data', data => {
                // UNCOMMENT THE FOLLOWING LINE TO DEBUG WHAT THE DEBUGGER IS DOING
                // console.log(data.toString())
                const text = data.toString();
                if (text.includes('Running')) {
                    clearTimeout(timeout);

                    // Just a little delay, 50ms
                    // Otherwise it may not yet be up as we only set up the
                    // server directly after the message.
                    // Alternatively, we could try-catch connect and retry a few times.
                    setTimeout(() => {
                        resolve();
                    }, 50)
                }
            });

            debuggerProcess.on('error', reject);
        });

        return ready.then(function () {
            dc = new DebugClient(undefined, undefined, 'haskell');
            return dc.start(port)
        })

    }));

    afterEach( () => {
      if (!debuggerProcess.killed)
        debuggerProcess.kill();
      dc.stop()
    });

    const mkHermetic = (path) => {
        const tmp = mkdtempSync(join(tmpdir(), "ghc-debugger-")) + path
        const data = process.cwd() + path;
        cpSync(data, tmp, { recursive: true }) // Copy data contents to temp directory
        return tmp
    }

    const mkConfig = config => {

        // Run tests on the temporary directory. This avoids issues with
        // hie-bios finding bad project roots because of cabal.projects in the
        // file system.
        const tmp = mkHermetic(config.projectRoot)
        config.projectRoot = tmp;

        return config
    }

    const simpleLaunchConfigs = [
        { name: "Vanilla config (no package)",
          config: mkConfig({
            projectRoot: "/data/simple",
            entryFile: "Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          })
        },
        { name: "Cabal config",
          config : mkConfig({
            projectRoot: "/data/cabal1",
            entryFile: "app/Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          })
        }
        // todo: Stack config?
        // todo: hie.yaml config?
    ]

    const basicTests = (launchCfg) => {

        // The most basic functionality we test on various different
        // configurations (such as Cabal vs without project vs Stack)
        // The remaining tests are run only on one config since the set up
        // starts being more specific (e.g. multiple home units with Cabal)
        describe("Most basic functionality", function () {

          describe(launchCfg.name, function () {

              it('should run program to the end', () => {
                  return Promise.all([
                      dc.configurationSequence(),
                      dc.launch(launchCfg.config),
                      dc.waitForEvent('exited').then(e => new Promise((resolve, reject) => {
                          if (e.body.exitCode == 0)
                              resolve(e)
                          else
                              reject(new Error("Expecting ExitCode 1"))
                      }))
                  ]);
              });

              it('should stop on a breakpoint', () => {
                  const expected = { path: launchCfg.config.projectRoot + "/" + launchCfg.config.entryFile, line: 6 }
                  return dc.hitBreakpoint(launchCfg.config, { path: launchCfg.config.entryFile, line: 6 }, expected, expected);
              });

              it('should stop on an exception', () => {

                  // const expected = { path: launchCfg.config.projectRoot + "/" + launchCfg.config.entryFile, line: 10 }
                  // Currently, no information is provided when stopped at an exception:
                  const expected = { }
                  return Promise.all([

                      dc.waitForEvent('initialized').then(event => {
                          return dc.setExceptionBreakpointsRequest({
                              filters: [ 'break-on-exception' ]
                          });
                      }).then(response => {
                          return dc.configurationDoneRequest();
                      }),

                      dc.launch(launchCfg.config),

                      dc.assertStoppedLocation('exception', expected)
                  ]);
              });

              // TODO: Break on a different module

              // TODO: Step in, step next, ...
              // TODO: Resume and hit next breakpoint
              // ...
          })
        })
    }

    const fetchLocalVars = async () => {
        const stResp = await dc.stackTraceRequest({ threadId: 0 });
        const sf0 = stResp.body.stackFrames[0];
        const scResp = await dc.scopesRequest({ frameId: sf0.id });
        const localsScope = scResp.body.scopes.find(scope => scope.name == "Locals");
        const variablesResp = await dc.variablesRequest({ variablesReference: localsScope.variablesReference });
        const variables = variablesResp.body.variables;
        return {
            all: variables,
            get: function (name) {
                const r = variables.find(v => v.name == name);
                assert(r, `Variable ${name} not found`);
                return r;
            }
        };
    }

    const forceLazy = async (v) => {
        assert.strictEqual(v.presentationHint.lazy, true, `Variable ${v.name} should be lazy`);
        assert.strictEqual(v.value, '_', `Variable ${v.name} should be "_" because it is lazy`);
        assert.notStrictEqual(v.variablesReference, 0, `Variable ${v.name} should be expandable (because it is lazy)`);

        // Force a lazy variable
        const forceResp = await dc.variablesRequest({ variablesReference: v.variablesReference });
        const forcedVar = forceResp.body.variables[0]
        return forcedVar
    }

    const expandVar = async (v) => {
        assert.notStrictEqual(v.variablesReference, 0, `Variable ${v.name} should be expandable (because it is a structure)`);

        // Expand a structure (similarly to forcing a lazy variable, but because it is not lazy it will fetch the fields)
        const childrenResp = await dc.variablesRequest({ variablesReference: v.variablesReference });
        const children = childrenResp.body.variables;
        return {
            all: children,
            get: function (name) {
                const r = children.find(v => v.name == name);
                assert(r, `Variable ${name} not found`);
                return r;
            }
        };
    }

    const assertIsString = (v, expected) => {
        assert.strictEqual(v.type, "String", `Variable ${v.name} should be a String`);
        assert.strictEqual(v.value, expected, `Variable ${v.name} should be "${expected}"`);
        assert.strictEqual(v.variablesReference, 0, `Variable ${v.name} should not be expandable (because it is a String)`);
    }

    simpleLaunchConfigs.forEach(basicTests);

    describe("Variable inspection tests", function () {

        it('ints and strings should be displayed as values', async () => {

            let config = mkConfig({
                  projectRoot: "/data/cabal1",
                  entryFile: "app/Main.hs",
                  entryPoint: "main",
                  entryArgs: ["some", "args"],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 15 }

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 15 }, expected, expected);

            const variables = await fetchLocalVars()

            // Int variables are displayed as ints
            const aVar = variables.get('a');
            const bVar = variables.get('b');

            assert.strictEqual(aVar.value, '2');
            assert.strictEqual(bVar.value, '4');

            // Force lazy variable 'c'
            // Strings are forced and displayed whole rather than as a structure
            const cVar = variables.get('c');
            const cVarForced = await forceLazy(cVar);

            assertIsString(cVarForced, '"call_fxxx"');

            // After a variable is forced, a new locals request is done. Check again for c == call_fxxx afterwards
            const refreshedVars = await fetchLocalVars();
            const refreshedCVar = refreshedVars.get('c');

            assertIsString(refreshedCVar, '"call_fxxx"')
        })

        it('allow arbitrarily deep inspection and strings are displayed as values arbitrarily deep (issue #8 and #9)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/simple2",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 19 }

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 19 }, expected, expected);

            // get the locals
            let locals = await fetchLocalVars();
            const pVar = await forceLazy(locals.get('p'));
            const pChild = await expandVar(pVar);
            const _1Var = await forceLazy(pChild.get('_1'));
            assertIsString(_1Var, '"d=1"');

            let focus = pChild.get('_2');

            // Now walk the spine from d=2 through d=6
            for (let expectedLevel = 2; expectedLevel <= 6; expectedLevel++) {
                const focusF = await forceLazy(focus);
                const children = await expandVar(focusF);
                const dChild = await forceLazy(children.get('_1'));
                assertIsString(dChild, `"d=${expectedLevel}"`);

                focus = children.get('_2');
            }

            // Finally, we should be at the OK constructor
            const focusF = await forceLazy(focus);
            assert.strictEqual(focusF.value, 'OK');
        })
    })


    describe("Stepping out", function () {

        // Mimics GHC's T26042b
        it('without tail calls', async () => {

            let config = mkConfig({
                  projectRoot: "/data/T6",
                  entryFile: "MainA.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = (line) => ({ path: config.projectRoot + "/" + config.entryFile, line: line });

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 9 }, expected(9), expected(9));

            // foo to bar
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(20));

            // bar back to foo
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(14));

            // back to main
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(6));

            // jump to thunk for `a`
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(18));
        })

        // Mimics GHC's T26042c
        it('with tail calls', async () => {

            let config = mkConfig({
                  projectRoot: "/data/T6",
                  entryFile: "MainB.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = (line) => ({ path: config.projectRoot + "/" + config.entryFile, line: line });

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 9 }, expected(9), expected(9));

            // step out of foo True and observe that we have skipped its call in bar,
            // and the call of bar in foo False.
            // we go straight to `main`.
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(6))

            // stepping out again jumps to the thunk for `a`
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(17))

            // more thunks
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(13))

        })
    })
})

