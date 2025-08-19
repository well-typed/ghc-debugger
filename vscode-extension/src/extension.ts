/*---------------------------------------------------------
 * Copyright (C) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------*/
/*
 * extension.ts contains code for launching the debug adapter in three different ways:
 * - as an external program communicating with VS Code via stdin/stdout,
 * - as a server process communicating with VS Code via sockets or named pipes, or
 * - as inlined code running in the extension itself (default).
 * 
 * Since the code in extension.ts uses node.js APIs it cannot run in the browser.
 */

'use strict';

import { WorkspaceFolder, DebugConfiguration, ProviderResult, CancellationToken } from 'vscode';
import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as net from 'net';

function getFreePort(): Promise<number> {
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

function spawnDebugger(logger: vscode.OutputChannel, port: number) {
    let adapterCmd = 'ghc-debug-adapter';

    try {
        // Get GHC version
        const ghcVersion = cp.execSync('ghc --numeric-version', { encoding: 'utf8' }).trim();
        const candidate = `ghc-debug-adapter-${ghcVersion}`;

        // Check if candidate is in PATH
        try {
            cp.execSync(process.platform === 'win32'
                ? `where ${candidate}`
                : `command -v ${candidate}`);

            logger.appendLine(`[Factory] Found debugger executable: ${candidate}`);

            adapterCmd = candidate; // if found, use versioned adapter
        } catch {
            // Not found, fall back to default
        }
    } catch (err) {
        console.warn("Could not determine GHC version, falling back to ghc-debug-adapter");
    }

    return cp.spawn(adapterCmd, ['--port', port.toString()]);
}

let logger : vscode.OutputChannel = vscode.window.createOutputChannel("ghc-debug-adapter");

export function activate(context: vscode.ExtensionContext) {

    // run the debug adapter as a server inside the extension and communicate via a socket
    let factory = new GHCDebugAdapterServerDescriptorFactory(context);

	context.subscriptions.push(
		vscode.commands.registerCommand('extension.haskell-debugger-extension.runEditorContents', (resource: vscode.Uri) => {
			let targetResource = resource;
			if (!targetResource && vscode.window.activeTextEditor) {
				targetResource = vscode.window.activeTextEditor.document.uri;
			}
			if (targetResource) {
				vscode.debug.startDebugging(undefined, {
					type: 'ghc-debugger',
					name: 'Run File',
					request: 'launch',
					entryFile: targetResource.fsPath,
                    entryPoint: 'main',
                    entryArgs: [],
                    extraGhcArgs: []
				},
					{ noDebug: true }
				);
			}
		}),
		vscode.commands.registerCommand('extension.haskell-debugger-extension.debugEditorContents', (resource: vscode.Uri) => {
			let targetResource = resource;
			if (!targetResource && vscode.window.activeTextEditor) {
				targetResource = vscode.window.activeTextEditor.document.uri;
			}
			if (targetResource) {
				vscode.debug.startDebugging(undefined, {
					type: 'ghc-debugger',
					name: 'Debug File',
					request: 'launch',
					entryFile: targetResource.fsPath,
                    entryPoint: 'main',
                    entryArgs: [],
                    extraGhcArgs: []
				});
			}
		}),
		vscode.commands.registerCommand('extension.haskell-debugger-extension.toggleFormatting', (variable) => {
			const ds = vscode.debug.activeDebugSession;
			if (ds) {
				ds.customRequest('toggleFormatting');
			}
		})
	);

	// register a configuration provider for 'ghc-debugger' debug type
	const provider = new GHCConfigurationProvider();
	context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ghc-debugger', provider));

	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ghc-debugger', factory));
	// if ('dispose' in factory) {
	// 	context.subscriptions.push(factory);
	// }

	// override VS Code's default implementation of the debug hover
	// here we match only Mock "variables", that are words starting with an '$'
	// context.subscriptions.push(vscode.languages.registerEvaluatableExpressionProvider('markdown', {
	// 	provideEvaluatableExpression(document: vscode.TextDocument, position: vscode.Position): vscode.ProviderResult<vscode.EvaluatableExpression> {

	// 		const VARIABLE_REGEXP = /\$[a-z][a-z0-9]*/ig;
	// 		const line = document.lineAt(position.line).text;

	// 		let m: RegExpExecArray | null;
	// 		while (m = VARIABLE_REGEXP.exec(line)) {
	// 			const varRange = new vscode.Range(position.line, m.index, position.line, m.index + m[0].length);

	// 			if (varRange.contains(position)) {
	// 				return new vscode.EvaluatableExpression(varRange);
	// 			}
	// 		}
	// 		return undefined;
	// 	}
	// }));

	// override VS Code's default implementation of the "inline values" feature"
	//context.subscriptions.push(vscode.languages.registerInlineValuesProvider('markdown', {

	//	provideInlineValues(document: vscode.TextDocument, viewport: vscode.Range, context: vscode.InlineValueContext) : vscode.ProviderResult<vscode.InlineValue[]> {

	//		const allValues: vscode.InlineValue[] = [];

	//		for (let l = viewport.start.line; l <= context.stoppedLocation.end.line; l++) {
	//			const line = document.lineAt(l);
	//			var regExp = /\$([a-z][a-z0-9]*)/ig;	// variables are words starting with '$'
	//			do {
	//				var m = regExp.exec(line.text);
	//				if (m) {
	//					const varName = m[1];
	//					const varRange = new vscode.Range(l, m.index, l, m.index + varName.length);

	//					// some literal text
	//					//allValues.push(new vscode.InlineValueText(varRange, `${varName}: ${viewport.start.line}`));

	//					// value found via variable lookup
	//					allValues.push(new vscode.InlineValueVariableLookup(varRange, varName, false));

	//					// value determined via expression evaluation
	//					//allValues.push(new vscode.InlineValueEvaluatableExpression(varRange, varName));
	//				}
	//			} while (m);
	//		}

	//		return allValues;
	//	}
	//}));

}

export function deactivate() {
	// nothing to do
}

class GHCDebugAdapterServerDescriptorFactory implements vscode.DebugAdapterDescriptorFactory {

    private processes = new Map<string, cp.ChildProcess>();
	private logger: vscode.OutputChannel;

	constructor(context: vscode.ExtensionContext) {
		this.logger = vscode.window.createOutputChannel("Haskell Debugger");
		this.logger.appendLine("[Factory] Initialized");

        context.subscriptions.push(
            vscode.debug.onDidTerminateDebugSession((session) => {
                const proc = this.processes.get(session.id);
                if (proc && !proc.killed) {
                    this.logger.appendLine(`[Factory] Killing process for session ${session.id}`);
                    proc.kill();
                }
                this.processes.delete(session.id);
            })
        );
	}

	async createDebugAdapterDescriptor(
		session: vscode.DebugSession,
		executable: vscode.DebugAdapterExecutable | undefined
	): Promise<vscode.DebugAdapterDescriptor> {

		// return new vscode.DebugAdapterServer(4717, 'localhost');

		const port = await getFreePort();
		this.logger.appendLine(`[Factory] Launching ghc-debugger on port ${port}`);

		// const debuggerProcess = cp.spawn('ghc-debug-adapter', ['--port', port.toString(), "+RTS", "-pj"], {cwd: "/var/folders/tv/35hlch6s3y15hfvndc71l6d40000gn/T/tmp.7Vsfzyoozc"});
		const debuggerProcess = spawnDebugger(logger, port);

        debuggerProcess.on('spawn', () => {
            this.logger.appendLine('[Factory] debugger executable spawned...');
        });

		debuggerProcess.stdout.on('data', (data) => {
			this.logger.appendLine(`[stdout] ${data}`);
		});

		debuggerProcess.stderr.on('data', (data) => {
			this.logger.appendLine(`[stderr] ${data}`);
		});

		debuggerProcess.on('exit', (code, signal) => {
			this.logger.appendLine(`[exit] ghc-debugger exited with code ${code} and signal ${signal}`);
		});

		debuggerProcess.on('error', (err) => {
			this.logger.appendLine(`[error] Failed to start ghc-debugger: ${err.message}`);
		});

		this.processes.set(session.id, debuggerProcess);

        const ready = new Promise<void>((resolve, reject) => {
            const timeout = setTimeout(() => {
                reject(new Error("ghc-debugger did not signal readiness in time"));
            }, 15000); // 15 second timeout

		    this.logger.appendLine(`[Factory] Waiting for debugger to be ready...`);
            debuggerProcess.stdout.on('data', (data: Buffer) => {
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

        await ready;
		this.logger.appendLine(`[Factory] Debugger is ready on port ${port}`);
		return new vscode.DebugAdapterServer(port, 'localhost');
	}

	dispose() {
		this.logger.appendLine("[Factory] Disposing and cleaning up processes...");
		for (const proc of this.processes.values()) {
			if (!proc.killed) {
				proc.kill();
			}
		}
		this.processes.clear();
		this.logger.dispose();
	}

}

class GHCConfigurationProvider implements vscode.DebugConfigurationProvider {

	/**
	 * Massage a debug configuration just before a debug session is being launched,
	 * e.g. add all missing attributes to the debug configuration.
	 */
	resolveDebugConfiguration(folder: WorkspaceFolder | undefined, config: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {

		// if launch.json is missing or empty
		if (!config.type && !config.request && !config.name) {
			const editor = vscode.window.activeTextEditor;
			if (editor && editor.document.languageId === 'markdown') {
				config.type = 'ghc-debugger';
				config.name = 'Launch';
				config.request = 'launch';
				config.entryFile = '${file}';
				config.entryPoint = 'main';
				config.entryArgs = [];
				config.extraGhcArgs = [];
			}
		}

		if (!config.entryFile) {
			return vscode.window.showInformationMessage("Cannot find an entry file to debug").then(_ => {
				return undefined;	// abort launch
			});
		}

		return config;
	}
}
