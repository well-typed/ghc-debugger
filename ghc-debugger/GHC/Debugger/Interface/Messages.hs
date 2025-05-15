{-# LANGUAGE DeriveGeneric,
             StandaloneDeriving,
             OverloadedStrings,
             DuplicateRecordFields,
             TypeApplications
             #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- JSON GHC.BreakpointId

-- | Types for sending and receiving messages to/from ghc-debugger
module GHC.Debugger.Interface.Messages where

import GHC.Generics
import Data.Aeson
import qualified GHC
import qualified GHC.Utils.Outputable as GHC
import GHC.Unit.Types
import Language.Haskell.Syntax.Module.Name

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | The commands sent to ghc debugger
data Command

  -- | Set a breakpoint on a given function, or module by line number
  = SetBreakpoint Breakpoint

  -- | Delete a breakpoint on a given function, or module by line number
  | DelBreakpoint Breakpoint

  -- | Find the valid breakpoints locations for the given module Breakpoint
  | GetBreakpointsAt Breakpoint

  -- | Clear all breakpoints in the specified file.
  -- This is useful because DAP's `setBreakpoints` re-sets all breakpoints from zero for a source rather than incrementally.
  | ClearModBreakpoints { file :: FilePath }

  -- | Clear all function breakpoints
  | ClearFunctionBreakpoints

  -- | Get the evaluation stacktrace until the current breakpoint.
  | GetStacktrace

  -- | Get the list of available scopes at the current breakpoint
  | GetScopes

  -- | Get the variables in scope for the current breakpoint.
  --
  -- Note: for GHCs <9.13 this only reports the variables free in the expression
  -- we're stopped at rather than all variables in scope.
  | GetVariables VariableReference

  -- | Evaluate an expression at the current breakpoint.
  | DoEval String

  -- | Continue executing from the current breakpoint
  | DoContinue

  -- | Step local, which executes until next breakpoint in the same function.
  | DoStepLocal

  -- | Single step always to the next breakpoint. Used for "step-in".
  | DoSingleStep

  -- | Step out to the breakpoint immediately following a return.
  | DoStepOut

  -- | Execute a prog with debugging enabled. Breaks on the existing breakpoints.
  --
  -- Constructed with an entry point function name and the arguments to pass it.
  --
  -- When the @'EntryPoint'@ is @'Main'@, @'runArgs'@ are set as process
  -- invocation arguments (as in @argv@) rather than passed directly as a
  -- Haskell function arguments.
  | DebugExecution { entryPoint :: EntryPoint, runArgs :: [String] }

  -- | Terminate ghc-debugger and exit
  | TerminateProcess

-- | An entry point for program execution.
data EntryPoint = MainEntry { mainName :: Maybe String } | FunctionEntry { fnName :: String }
  deriving (Show, Generic)

-- | A breakpoint can be set/removed on functions by name, or in modules by
-- line number. And, globally, for all exceptions, or just uncaught exceptions.
data Breakpoint
  = ModuleBreak { path :: FilePath, lineNum :: Int, columnNum :: Maybe Int }
  | FunctionBreak { function :: String }
  | OnExceptionsBreak
  | OnUncaughtExceptionsBreak
  deriving (Show, Generic)

-- | Information about a scope
data ScopeInfo = ScopeInfo
      { kind :: ScopeVariablesReference
      , sourceSpan :: SourceSpan
      , numVars :: Maybe Int
      , expensive :: Bool }
  deriving (Show, Generic)

data VarFields = LabeledFields [VarInfo]
               | IndexedFields [VarInfo]
               | NoFields
               deriving (Show, Generic, Eq)

-- | Information about a variable
data VarInfo = VarInfo
      { varName  :: String
      , varType  :: String
      , varValue :: String
      , isThunk  :: Bool
      , varRef   :: VariableReference
      -- ^ A reference back to this variable

      -- TODO:
      --  memory reference using ghc-debug.
      }
      deriving (Show, Generic, Eq)

-- | What kind of breakpoint are we referring to, module or function breakpoints?
-- Used e.g. in the 'ClearBreakpoints' request
data BreakpointKind
  -- | Module breakpoints
  = ModuleBreakpointKind
  -- | Function breakpoints
  | FunctionBreakpointKind
  deriving (Show, Generic, Eq)

-- | Referring to existing scopes
data ScopeVariablesReference
  = LocalVariablesScope
  | ModuleVariablesScope
  | GlobalVariablesScope
  deriving (Show, Generic, Eq, Ord)

-- | The type of variables referenced, or a particular variable referenced for its fields or value (when inspecting a thunk)
data VariableReference
  -- | A void reference to nothing at all. Used e.g. for ty cons and data cons
  = NoVariables

  -- | Variables in the local context (includes arguments, previous bindings)
  | LocalVariables

  -- | Variables in the module where we're stopped
  | ModuleVariables

  -- | Variables in the global context
  | GlobalVariables

  -- | A reference to a specific variable.
  -- Used to force its result or get its structured children
  | SpecificVariable Int

  deriving (Show, Generic, Eq, Ord)

instance Bounded VariableReference where
  minBound = NoVariables
  maxBound = SpecificVariable maxBound

instance Enum VariableReference where
  toEnum 0 = NoVariables
  toEnum 1 = LocalVariables
  toEnum 2 = ModuleVariables
  toEnum 3 = GlobalVariables
  toEnum n = SpecificVariable (n - 4)

  fromEnum NoVariables          = 0
  fromEnum LocalVariables       = 1
  fromEnum ModuleVariables      = 2
  fromEnum GlobalVariables      = 3
  fromEnum (SpecificVariable n) = 4 + n

-- | A source span type for the interface. Like 'RealSrcSpan'.
data SourceSpan = SourceSpan
      { file :: !FilePath
      -- ^ Path to file where this span is located
      , startLine :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan start line
      , endLine :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan end line
      , startCol :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan start col
      , endCol :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan end col
      }
      deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

-- | The responses sent by `ghc-debugger` to the client
data Response
  = DidEval EvalResult
  | DidSetBreakpoint BreakFound
  | DidRemoveBreakpoint BreakFound
  | DidGetBreakpoints (Maybe SourceSpan)
  | DidClearBreakpoints
  | DidContinue EvalResult
  | DidStep EvalResult
  | DidExec EvalResult
  | GotStacktrace [StackFrame]
  | GotScopes [ScopeInfo]
  | GotVariables (Either VarInfo [VarInfo])
  | Aborted String
  | Initialised

data BreakFound
  = BreakFound
    { changed :: !Bool
    -- ^ Did the status of the found breakpoint change?
    , breakId :: GHC.BreakpointId
    -- ^ Internal breakpoint identifier (module + ix) (TODO: Don't expose GHC)
    , sourceSpan :: SourceSpan
    -- ^ Source span for interface
    }
  -- | Breakpoint found but without location info.
  -- This happens when setting breakpoints on exceptions.
  | BreakFoundNoLoc
    { changed :: Bool }
  -- | No breakpoints found
  | BreakNotFound
  -- | Found many breakpoints.
  -- Caused by setting breakpoint on a name with multiple matches or many equations.
  | ManyBreaksFound [BreakFound]
  deriving (Show, Generic)

data EvalResult
  = EvalCompleted { resultVal :: String, resultType :: String }
  | EvalException { resultVal :: String, resultType :: String }
  | EvalStopped   { breakId :: Maybe GHC.BreakpointId {-^ Did we stop at an exception (@Nothing@) or at a breakpoint (@Just@)? -} }
  -- | Evaluation failed for some reason other than completed/completed-with-exception/stopped.
  | EvalAbortedWith String
  deriving (Show, Generic)

data StackFrame
  = StackFrame
    { name :: String
    -- ^ Title of stack frame
    , sourceSpan :: SourceSpan
    -- ^ Source span for this stack frame
    }
  deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

deriving instance Show Command
deriving instance Generic Command

deriving instance Show Response
deriving instance Generic Response

instance ToJSON Command    where toEncoding = genericToEncoding defaultOptions
instance ToJSON Breakpoint where toEncoding = genericToEncoding defaultOptions
instance ToJSON BreakpointKind where toEncoding = genericToEncoding defaultOptions
instance ToJSON ScopeVariablesReference where toEncoding = genericToEncoding defaultOptions
instance ToJSON VariableReference where toEncoding = genericToEncoding defaultOptions
instance ToJSON Response   where toEncoding = genericToEncoding defaultOptions
instance ToJSON EvalResult where toEncoding = genericToEncoding defaultOptions
instance ToJSON BreakFound where toEncoding = genericToEncoding defaultOptions
instance ToJSON SourceSpan where toEncoding = genericToEncoding defaultOptions
instance ToJSON EntryPoint where toEncoding = genericToEncoding defaultOptions
instance ToJSON StackFrame where toEncoding = genericToEncoding defaultOptions
instance ToJSON ScopeInfo  where toEncoding = genericToEncoding defaultOptions
instance ToJSON VarInfo    where toEncoding = genericToEncoding defaultOptions
instance ToJSON VarFields where toEncoding = genericToEncoding defaultOptions

instance FromJSON Command
instance FromJSON Breakpoint
instance FromJSON BreakpointKind
instance FromJSON ScopeVariablesReference
instance FromJSON VariableReference
instance FromJSON Response
instance FromJSON EvalResult
instance FromJSON BreakFound
instance FromJSON SourceSpan
instance FromJSON EntryPoint
instance FromJSON StackFrame
instance FromJSON ScopeInfo
instance FromJSON VarInfo
instance FromJSON VarFields

instance Show GHC.BreakpointId where
  show (GHC.BreakpointId m ix) = "BreakpointId " ++ GHC.showPprUnsafe m ++ " " ++ show ix

instance ToJSON GHC.BreakpointId where
  toJSON (GHC.BreakpointId (Module unit mn) ix) =
    object [ "module_name" .= moduleNameString mn
           , "module_unit" .= unitString unit
           , "ix" .= ix
           ]
instance FromJSON GHC.BreakpointId where
  parseJSON = withObject "BreakpointId" $ \v -> GHC.BreakpointId
        <$> (Module <$> (stringToUnit <$> v .: "module_unit") <*> (mkModuleName <$> v .: "module_name"))
        <*> v .: "ix"
