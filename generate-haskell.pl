#!/usr/bin/env perl

use strict;
use File::Basename;
use Template;

# The destinations are all relative to `GhcUser`.
my %modules = (
    'Annotations' => { name => 'Annotations' },
    'ApiAnnotation' => { name => 'APIAnnotation' },
    'Ar' => {
	name => 'Archive',
	doc => '__The need for "GhcUser.Archive"__
--
-- Building @-staticlib@ required the presence of libtool, and was a such restricted to mach-o only. As libtool on macOS and gnu libtool are very different, there was no simple portable way to support this.
--
-- libtool for static archives does essentially: concatinate the input archives, add the input objects, and create a symbol index. Using @ar@ for this task fails as even @ar@ (bsd and gnu, llvm, ...) do not provide the same features across platforms (e.g. index prefixed retrieval of objects with the same name.)
--
-- As Archives are rather simple structurally, we can just build the archives with Haskell directly and use ranlib on the final result to get the symbol index. This should allow us to work around with the differences/abailability of libtool across differet platforms.'
    },
    'AsmCodeGen' => {
	name => 'Assembler.CodeGen',
	doc => 'This is the top-level module in the native code generator.'
    },
    'AsmUtils' => { name => 'Assembler.Utilities' },
    'Avail' => { name => 'Availability' },
    'Bag' => {
	name => 'Data.Bag',
	doc => 'An unordered collection with duplicates.'
    },
    'BasicTypes' => {
	name => 'BasicTypes',
	doc => '__Miscellanous types__
--
-- This module defines a miscellaneously collection of very simple types that
--
-- * have no other obvious home,
-- * don\'t depend on any other complicated types, and
-- * are used in more than one "part" of the compiler.'
    },
    'BinFingerprint' => { name => 'Binary.Fingerprint' },
    'BinIface' => { name => 'Binary.InterfaceFile' },
    'Binary' => { name => 'Binary' },
    'Bitmap' => { name => 'Data.Bitmap' },
    'BkpSyn' => { name => 'Backpack.Syntax' },
    'BlockId' => { name => 'BlockId' },
    'BooleanFormula' => { name => 'BooleanFormula' },
    'BufWrite' => { name => 'Handles.WriteBuffered' },
    'BuildTyCl' => { name => 'Build.TypeClass' },
    'ByteCodeAsm' => { name => 'ByteCode.Assembler' },
    'ByteCodeGen' => { name => 'ByteCode.Generator' },
    'ByteCodeInstr' => { name => 'ByteCode.Instructions' },
    'ByteCodeItbls' => { name => 'ByteCode.Infotables' },
    'ByteCodeLink' => { name => 'ByteCode.Linker' },
    'ByteCodeTypes' => { name => 'ByteCode.Types' },
    'CLabel' => { name => 'C.Label' },
    'CPrim' => { name => 'C.Primitives' },
    'CSE' => { name => 'CommonSubexpressionElimination' },
    'CallArity' => { name => 'CallArity' },
    'CgUtils' => { name => 'CodeGen.Utilities' },
    'Check' => { name => 'Check' },
    'Class' => { name => 'Type.Class' },
    'CmdLineParser' => { name => 'CommandLineParser' },
    'Cmm' => { name => 'Cmm' },
    'CmmBuildInfoTables' => { name => 'Cmm.BuildInfoTables' },
    'CmmCallConv' => { name => 'Cmm.CallingConvention' },
    'CmmCommonBlockElim' => { name => 'Cmm.CommonBlockElimination' },
    'CmmContFlowOpt' => { name => 'Cmm.ControlFlowOptimizations' },
    'CmmExpr' => { name => 'Cmm.Expression' },
    'CmmImplementSwitchPlans' => { name => 'Cmm.ImplementSwitchPlans' },
    'CmmInfo' => { name => 'Cmm.Info' },
    'CmmLayoutStack' => { name => 'Cmm.LayoutStack' },
    'CmmLex' => { name => 'Cmm.Lexer' },
    'CmmLint' => { name => 'Cmm.Linter' },
    'CmmLive' => { name => 'Cmm.Live' },
    'CmmMachOp' => { name => 'Cmm.MachineOperations' },
    'CmmMonad' => { name => 'Cmm.Monad' },
    'CmmNode' => { name => 'Cmm.Node' },
    'CmmOpt' => { name => 'Cmm.Optimizations' },
    'CmmParse' => { name => 'Cmm.Parser' },
    'CmmPipeline' => { name => 'Cmm.Pipeline' },
    'CmmProcPoint' => { name => 'Cmm.ProcPoint' },
    'CmmSink' => { name => 'Cmm.Sink' },
    'CmmSwitch' => { name => 'Cmm.Switch' },
    'CmmType' => { name => 'Cmm.Type' },
    'CmmUtils' => { name => 'Cmm.Utilities' },
    'CoAxiom' => { name => 'Type.Coercion.Axioms' },
    'CodeGen.Platform' => { name => 'Platform.CodeGen' },
    'CodeGen.Platform.ARM' => { name => 'Platform.ARM.CodeGen' },
    'CodeGen.Platform.ARM64' => { name => 'Platform.ARM64.CodeGen' },
    'CodeGen.Platform.NoRegs' => { name => 'Platform.Unregistered.CodeGen' },
    'CodeGen.Platform.PPC' => { name => 'Platform.PPC.CodeGen' },
    'CodeGen.Platform.PPC_Darwin' => { name => 'Platform.PPC_Darwin.CodeGen' },
    'CodeGen.Platform.SPARC' => { name => 'Platform.SPARC.CodeGen' },
    'CodeGen.Platform.X86' => { name => 'Platform.X86.CodeGen' },
    'CodeGen.Platform.X86_64' => { name => 'Platform.X86_64.CodeGen' },
    'CodeOutput' => { name => 'CodeOutput' },
    'Coercion' => { name => 'Type.Coercion' },
    'ConLike' => { name => 'ConstructorLike' },
    'Config' => { name => 'Config' },
    'Constants' => { name => 'Constants' },
    'Convert' => { name => 'Convert' },
    'CoreArity' => { name => 'Core.Arity' },
    'CoreFVs' => { name => 'Core.FreeVariables' },
    'CoreLint' => { name => 'Core.Linter' },
    'CoreMap' => { name => 'Core.Map' },
    'CoreMonad' => { name => 'Core.Monad' },
    'CoreOpt' => { name => 'Core.Optimizer' },
    'CorePrep' => { name => 'Core.Prepare' },
    'CoreSeq' => { name => 'Core.Seq' },
    'CoreStats' => { name => 'Core.Statistics' },
    'CoreSubst' => { name => 'Core.Substitutions' },
    'CoreSyn' => {
	name => 'Core.Syntax',
	doc => 'Holds all the main data types for use by for the Glasgow Haskell Compiler midsection.',
	rename => [
	    { old => 'expandUnfolding_maybe', new => 'expandUnfolding' },
	    { old => 'exprToCoercion_maybe', new => 'exprToCoercion' },
	    ]
    },
    'CoreTidy' => { name => 'Core.Tidy' },
    'CoreToStg' => { name => 'Core.ToSTG' },
    'CoreUnfold' => { name => 'Core.Unfold' },
    'CoreUtils' => { name => 'Core.Utilities' },
    'CostCentre' => { name => 'CostCentre' },
    'CostCentreState' => { name => 'CostCentre.State' },
    'Coverage' => { name => 'Coverage' },
    'Ctype' => { name => 'CharacterType' },
    'DataCon' => { name => 'DataConstructor' },
    'Debug' => {
	name => 'Debug',
	doc => 'Debugging data
--
-- Association of debug data on the Cmm level, with methods to encode it in event log format for later inclusion in profiling event logs.'
    },
    'Debugger' => {
	name => 'GHC.Interpreter.Debugger',
	doc => 'GHCi Interactive debugging commands'
    },
    'Demand' => { name => 'Demand' },
    'Desugar' => { name => 'Desugaring' },
    'Digraph' => { name => 'Data.DirectedGraph' },
    'DmdAnal' => { name => 'Demand.Analysis' },
    'DriverBkp' => { name => 'Driver.Backpack' },
    'DriverMkDepend' => { name => 'Driver.Makefile.Dependency' },
    'DriverPhases' => { name => 'Driver.Phases' },
    'DriverPipeline' => { name => 'Driver.Pipeline' },
    'DsArrows' => { name => 'Desugaring.Arrows' },
    'DsBinds' => {
	name => 'Desugaring.Bindings',
	doc => 'Pattern-matching bindings (HsBinds and MonoBinds)
--
-- Handles "GhcUser.Haskell.Bindings"; those at the top level require different handling, in that the `GhcUser.Core.Syntax.Rec`/`GhcUser.Core.Syntax.NonRec`/etc. structure is thrown away (whereas at lower levels it is preserved with @let@/@letrec@s).',
    },
    'DsCCall' => { name => 'Desugaring.CCall' },
    'DsExpr' => { name => 'Desugaring.Expression' },
    'DsForeign' => { name => 'Desugaring.ForeignDeclarations' },
    'DsGRHSs' => { name => 'Desugaring.GuardedRightHandSides' },
    'DsListComp' => { name => 'Desugaring.Comprehensions' },
    'DsMeta' => { name => 'Desugaring.Meta' },
    'DsMonad' => {
	name => 'Desugaring.Monad',
	doc => 'monadery used in desugaring',
    },
    'DsUsage' => { name => 'Desugaring.Usage' },
    'DsUtils' => { name => 'Desugaring.Utilities' },
    'Dwarf' => { name => 'Format.DWARF' },
    'Dwarf.Constants' => { name => 'Format.DWARF.Constants' },
    'Dwarf.Types' => { name => 'Format.DWARF.Types' },
    'DynFlags' => { name => 'DynamicFlags' },
    'DynamicLoading' => { name => 'DynamicLoading' },
    'Elf' => { name => 'Format.ELF' },
    'Encoding' => { name => 'Character.Encooding' },
    'EnumSet' => { name => 'Data.EnumSet' },
    'ErrUtils' => {
	name => 'Error.Utilities',
	doc => 'Utilities for error reporting',
    },
    'Exception' => { name => 'Exception' },
    'Exitify' => { name => 'Exitify' },
    'ExtractDocs' => { name => 'ExtractDocs' },
    'FV' => { name => 'FreeVariables' },
    'FamInst' => { name => 'FamilyInstances' },
    'FamInstEnv' => { name => 'FamilyInstances.Environment' },
    'FastFunctions' => { name => 'Fast.Functions' },
    'FastMutInt' => { name => 'Fast.MutableInt' },
    'FastString' => { name => 'Fast.String' },
    'FastStringEnv' => { name => 'Fast.String.Environment' },
    'FieldLabel' => { name => 'FieldLabel' },
    'FileCleanup' => { name => 'FileCleanup' },
    'Finder' => { name => 'Module.Finder' },
    'Fingerprint' => { name => 'Fingerprint' },
    'FiniteMap' => { name => 'Data.FiniteMap' },
    'FlagChecker' => { name => 'FlagChecker' },
    'FloatIn' => { name => 'Float.In' },
    'FloatOut' => { name => 'Float.Out' },
    'ForeignCall' => { name => 'ForeignCall' },
    'Format' => { name => 'Format' },
    'FunDeps' => { name => 'FunctionalDependencies' },
    'GHC' => { name => 'GHC' },
    'GHCi' => { name => 'GHC.Interpreter' },
    'GhcMake' => { name => 'GHC.Make' },
    'GhcMonad' => { name => 'GHC.Monad' },
    'GhcPlugins' => {
	name => 'GHC.Plugins',
	doc => 'This module is not used by GHC itself. Rather, it exports all of the functions and types you are likely to need when writing a plugin for GHC. So authors of plugins can probably get away simply with saying @import "GhcUser.GHC.Plugins"@.
--
--   Particularly interesting modules for plugin writers include "GhcUser.Core.Syntax" and "GhcUser.Core.Monad".',
	hidden => ['nameModule'],
	rename => [
	    { old => 'getTyVar_maybe', new => 'getTyVar' },
	    { old => 'nameModule_maybe', new => 'nameModule' },
	    { old => 'splitAppTy_maybe', new => 'splitAppTy'},
	    { old => 'splitForAllTy_maybe', new => 'splitForAllTy'},
	    { old => 'tyConAppTyCon_maybe', new => 'tyConAppTyCon'},
	    { old => 'tyConAppArgs_maybe', new => 'tyConAppArgs'},
	    ]
    },
    'GraphBase' => { name => 'Graph.Base' },
    'GraphColor' => { name => 'Graph.Coloring' },
    'GraphOps' => { name => 'Graph.Operations' },
    'GraphPpr' => { name => 'Graph.PrettyPrint' },
    'HaddockUtils' => { name => 'Haddock.Utilities' },
    'HeaderInfo' => { name => 'HeaderInfo' },
    'Hooks' => { name => 'Hooks' },
    'Hoopl.Block' => { name => 'Hoopl.Block' },
    'Hoopl.Collections' => { name => 'Hoopl.Collections' },
    'Hoopl.Dataflow' => { name => 'Hoopl.Dataflow' },
    'Hoopl.Graph' => { name => 'Hoopl.Graph' },
    'Hoopl.Label' => { name => 'Hoopl.Label' },
    'HsBinds' => { name => 'Haskell.Bindings' },
    'HsDecls' => { name => 'Haskell.Declarations' },
    'HsDoc' => { name => 'Haskell.DocumentationString' },
    'HsDumpAst' => { name => 'Haskell.DumpAST' },
    'HsExpr' => { name => 'Haskell.Expression' },
    'HsExtension' => { name => 'Haskell.Extension' },
    'HsImpExp' => { name => 'Haskell.ImportExport' },
    'HsInstances' => { name => 'Haskell.Instances' },
    'HsLit' => { name => 'Haskell.Literal' },
    'HsPat' => { name => 'Haskell.Pattern' },
    'HsSyn' => { name => 'Haskell.Syntax' },
    'HsTypes' => { name => 'Haskell.Types' },
    'HsUtils' => { name => 'Haskell.Utilities' },
    'HscMain' => { name => 'Haskell.Compiler.Main' },
    'HscStats' => { name => 'Haskell.Compiler.Statistics' },
    'HscTypes' => { name => 'Haskell.Compiler.Types' },
    'IOEnv' => { name => 'IOEnvironment' },
    'Id' => { name => 'Id' },
    'IdInfo' => { name => 'Id.Info' },
    'IfaceEnv' => { name => 'Interface.Environment' },
    'IfaceSyn' => { name => 'Interface.Syntax' },
    'IfaceType' => { name => 'Interface.Type' },
    'Inst' => { name => 'Instance' },
    'InstEnv' => { name => 'Instance.Environment' },
    'Instruction' => { name => 'Instruction' },
    'InteractiveEval' => { name => 'InteractiveEval' },
    'InteractiveEvalTypes' => { name => 'InteractiveEval.Types' },
    'Json' => { name => 'JSON' },
    'Kind' => { name => 'Type.Kind' },
    'KnownUniques' => { name => 'Unique.Known' },
    'Lexeme' => { name => 'Lexeme' },
    'Lexer' => { name => 'Lexer' },
    'LiberateCase' => { name => 'LiberateCase' },
    'Linker' => { name => 'Linker' },
    'ListSetOps' => { name => 'Data.ListSet.Operations' },
    'ListT' => { name => 'Data.ListT' },
    'Literal' => { name => 'Literal' },
    'Llvm' => { name => 'LLVM' },
    'Llvm.AbsSyn' => { name => 'LLVM.AbstractSyntax' },
    'Llvm.MetaData' => { name => 'LLVM.Metadata' },
    'Llvm.PpLlvm' => { name => 'LLVM.PrettyPrint' },
    'Llvm.Types' => { name => 'LLVM.Types' },
    'LlvmCodeGen' => { name => 'LLVM.CodeGen' },
    'LlvmCodeGen.Base' => { name => 'LLVM.CodeGen.Base' },
    'LlvmCodeGen.CodeGen' => { name => 'LLVM.CodeGen.CodeGen' },
    'LlvmCodeGen.Data' => { name => 'LLVM.CodeGen.Data' },
    'LlvmCodeGen.Ppr' => { name => 'LLVM.CodeGen.PrettyPrint' },
    'LlvmCodeGen.Regs' => { name => 'LLVM.CodeGen.Registers' },
    'LlvmMangler' => { name => 'LLVM.Mangler' },
    'LoadIface' => { name => 'Interface.Load' },
    'Match' => { name => 'Match' },
    'MatchCon' => { name => 'Match.Constructors' },
    'MatchLit' => { name => 'Match.Literals' },
    'Maybes' => { name => 'Data.Maybe' },
    'MkCore' => { name => 'Core.Makers' },
    'MkGraph' => { name => 'Graph.Makers' },
    'MkId' => { name => 'Id.Makers' },
    'MkIface' => { name => 'Interface.Makers' },
    'Module' => { name => 'Module' },
    'MonadUtils' => { name => 'Monad.Utilities' },
    'NCGMonad' => { name => 'NativeCodeGenerator.Monad' },
    'Name' => { name => 'Name' },
    'NameCache' => { name => 'Name.Cache' },
    'NameEnv' => { name => 'Name.Env' },
    'NameSet' => { name => 'Name.Set' },
    'NameShape' => { name => 'Name.Shape' },
    'OccName' => { name => 'Occurence.Name' },
    'OccurAnal' => { name => 'Occurence.Analysis' },
    'OptCoercion' => { name => 'Type.Coercion.Optimization' },
    'OrdList' => {
	name => 'Data.OrderedList',
	doc => 'This is useful, general stuff for the Native Code Generator.
--
-- Provide trees (of instructions), so that lists of instructions can be appended in linear time.'
    },
    'Outputable' => { name => 'Outputable' },
    'PIC' => { name => 'PIC' },
    'PPC.CodeGen' => { name => 'Platform.PPC.CodeGen.CodeGen' },
    'PPC.Cond' => { name => 'Platform.PPC.Conditional' },
    'PPC.Instr' => { name => 'Platform.PPC.Instructions' },
    'PPC.Ppr' => { name => 'Platform.PPC.PrettyPrint' },
    'PPC.RegInfo' => { name => 'Platform.PPC.Registers.Info' },
    'PPC.Regs' => { name => 'Platform.PPC.Registers' },
    'PackageConfig' => { name => 'Packages.Config' },
    'Packages' => { name => 'Packages' },
    'Pair' => {
	name => 'Data.Pair',
	doc => 'A simple homogeneous pair type with useful `Functor`, `Applicative`, and `Traversable` instances.'
    },
    'Panic' => { name => 'Panic' },
    'Parser' => { name => 'Parser' },
    'PatSyn' => { name => 'PatternSynonym' },
    'PipelineMonad' => { name => 'Pipeline.Monad' },
    'PlaceHolder' => { name => 'Placeholder' },
    'Platform' => { name => 'Platform' },
    'PlatformConstants' => { name => 'Platform.Constants' },
    'Plugins' => { name => 'Plugins' },
    'PmExpr' => { name => 'PatternMatch.Expression' },
    'PprBase' => { name => 'PrettyPrint.Base' },
    'PprC' => { name => 'PrettyPrint.C' },
    'PprCmm' => { name => 'PrettyPrint.Cmm' },
    'PprCmmDecl' => { name => 'PrettyPrint.Cmm.Declaration' },
    'PprCmmExpr' => { name => 'PrettyPrint.Cmm.Expression' },
    'PprColour' => { name => 'PrettyPrint.Colour' },
    'PprCore' => { name => 'PrettyPrint.Core' },
    'PprTyThing' => { name => 'PrettyPrint.TypecheckableThing' },
    'PrelInfo' => { name => 'Prelude.Info' },
    'PrelNames' => { name => 'Prelude.Names' },
    'PrelRules' => { name => 'Prelude.Rules' },
    'Pretty' => { name => 'PrettyPrint.Combinators' },
    'PrimOp' => { name => 'PrimitiveOperations' },
    'ProfInit' => { name => 'ProfilingInit' },
    'RdrHsSyn' => {
	name => 'Reader.Haskell.Syntax',
	doc => 'Functions over "GhcUser.Haskell.Syntax" specialised to `GhcUser.Reader.Name.RdrName`.'
    },
    'RdrName' => { name => 'Reader.Name' },
    'Reg' => { name => 'Register' },
    'RegAlloc.Graph.ArchBase' => { name => 'Register.Allocator.Graph.Architecture.Base' },
    'RegAlloc.Graph.ArchX86' => { name => 'Register.Allocator.Graph.Architecture.X86' },
    'RegAlloc.Graph.Coalesce' => { name => 'Register.Allocator.Graph.Coalesce' },
    'RegAlloc.Graph.Main' => { name => 'Register.Allocator.Graph.Main' },
    'RegAlloc.Graph.Spill' => { name => 'Register.Allocator.Graph.Spill' },
    'RegAlloc.Graph.SpillClean' => { name => 'Register.Allocator.Graph.Spill.Clean' },
    'RegAlloc.Graph.SpillCost' => { name => 'Register.Allocator.Graph.Spill.Cost' },
    'RegAlloc.Graph.Stats' => { name => 'Register.Allocator.Graph.Statistics' },
    'RegAlloc.Graph.TrivColorable' => { name => 'Register.Allocator.Graph.TriviallyColourable' },
    'RegAlloc.Linear.Base' => { name => 'Register.Allocator.Linear.Base' },
    'RegAlloc.Linear.FreeRegs' => { name => 'Register.Allocator.Linear.FreeRegisters' },
    'RegAlloc.Linear.JoinToTargets' => { name => 'Register.Allocator.Linear.JoinToTargets' },
    'RegAlloc.Linear.Main' => { name => 'Register.Allocator.Linear.Main' },
    'RegAlloc.Linear.PPC.FreeRegs' => { name => 'Platform.PPC.Register.Allocator.Linear.FreeRegisters' },
    'RegAlloc.Linear.SPARC.FreeRegs' => { name => 'Platform.SPARC.Register.Allocator.Linear.FreeRegisters' },
    'RegAlloc.Linear.StackMap' => { name => 'Register.Allocator.Linear.StackMap' },
    'RegAlloc.Linear.State' => { name => 'Register.Allocator.Linear.State' },
    'RegAlloc.Linear.Stats' => { name => 'Register.Allocator.Linear.Statistics' },
    'RegAlloc.Linear.X86.FreeRegs' => { name => 'Platform.X86.Register.Allocator.Linear.FreeRegisters' },
    'RegAlloc.Linear.X86_64.FreeRegs' => { name => 'Platform.X86_64.Register.Allocator.Linear.FreeRegisters' },
    'RegAlloc.Liveness' => { name => 'Register.Allocator.Liveness' },
    'RegClass' => { name => 'Register.Class' },
    'RepType' => { name => 'TypeRepresentation' },
    'RnBinds' => { name => 'Renaming.Bindings' },
    'RnEnv' => {
	name => 'Renaming.Environment',
	doc => 'functions which convert `GhcUser.Reader.Name.RdrName`s into `GhcUser.Name.Name`s.',
    },
    'RnExpr' => { name => 'Renaming.Expression' },
    'RnFixity' => { name => 'Renaming.Fixity' },
    'RnHsDoc' => { name => 'Renaming.Haskell.DocumentationString' },
    'RnModIface' => { name => 'Renaming.ModuleInterface' },
    'RnNames' => { name => 'Renaming.Names' },
    'RnPat' => { name => 'Renaming.Pattern' },
    'RnSource' => { name => 'Renaming.Source' },
    'RnSplice' => { name => 'Renaming.Splice' },
    'RnTypes' => { name => 'Renaming.Types' },
    'RnUnbound' => { name => 'Renaming.Unbound' },
    'RnUtils' => { name => 'Renaming.Utilities' },
    'RtClosureInspect' => { name => 'RuntimeClosureInspection' },
    'Rules' => { name => 'Rules' },
    'SAT' => { name => 'StaticArgumentTransformation' },
    'SMRep' => { name => 'StorageManagerRepresentation' },
    'SPARC.AddrMode' => { name => 'Platform.SPARC.AddressMode' },
    'SPARC.Base' => { name => 'Platform.SPARC.Base' },
    'SPARC.CodeGen' => { name => 'Platform.SPARC.CodeGen.CodeGen' },
    'SPARC.CodeGen.Amode' => { name => 'Platform.SPARC.CodeGen.AddressMode' },
    'SPARC.CodeGen.Base' => { name => 'Platform.SPARC.CodeGen.Base' },
    'SPARC.CodeGen.CondCode' => { name => 'Platform.SPARC.CodeGen.ConditionCode' },
    'SPARC.CodeGen.Expand' => { name => 'Platform.SPARC.CodeGen.Expand' },
    'SPARC.CodeGen.Gen32' => { name => 'Platform.SPARC.CodeGen.Gen32' },
    'SPARC.CodeGen.Gen64' => { name => 'Platform.SPARC.CodeGen.Gen64' },
    'SPARC.CodeGen.Sanity' => { name => 'Platform.SPARC.CodeGen.Sanity' },
    'SPARC.Cond' => { name => 'Platform.SPARC.Condition' },
    'SPARC.Imm' => { name => 'Platform.SPARC.ImmediateValue' },
    'SPARC.Instr' => { name => 'Platform.SPARC.Instruction' },
    'SPARC.Ppr' => { name => 'Platform.SPARC.PrettyPrint' },
    'SPARC.Regs' => { name => 'Platform.SPARC.Registers' },
    'SPARC.ShortcutJump' => { name => 'Platform.SPARC.ShortcutJump' },
    'SPARC.Stack' => { name => 'Platform.SPARC.Stack' },
    'SetLevels' => { name => 'SetLevels' },
    'SimplCore' => { name => 'Simplifier.Core' },
    'SimplEnv' => { name => 'Simplifier.Environment' },
    'SimplMonad' => { name => 'Simplifier.Monad' },
    'SimplStg' => { name => 'Simplifier.SharedTermGraph' },
    'SimplUtils' => { name => 'Simplifier.Utilities' },
    'Simplify' => { name => 'Simplifier' },
    'SpecConstr' => { name => 'Specialise.Constructors' },
    'Specialise' => { name => 'Specialise' },
    'SrcLoc' => { name => 'SourceLocation' },
    'State' => { name => 'Data.State' },
    'StaticPtrTable' => { name => 'StaticPointerTable' },
    'StgCmm' => { name => 'SharedTermGraph.Cmm' },
    'StgCmmArgRep' => { name => 'SharedTermGraph.Cmm.ArgumentRepresentations' },
    'StgCmmBind' => { name => 'SharedTermGraph.Cmm.Bindings' },
    'StgCmmClosure' => { name => 'SharedTermGraph.Cmm.Closure' },
    'StgCmmCon' => { name => 'SharedTermGraph.Cmm.Constructors' },
    'StgCmmEnv' => { name => 'SharedTermGraph.Cmm.Environment' },
    'StgCmmExpr' => { name => 'SharedTermGraph.Cmm.Expression' },
    'StgCmmExtCode' => { name => 'SharedTermGraph.Cmm.ExtendedCode' },
    'StgCmmForeign' => { name => 'SharedTermGraph.Cmm.ForeignCalls' },
    'StgCmmHeap' => { name => 'SharedTermGraph.Cmm.Heap' },
    'StgCmmHpc' => { name => 'SharedTermGraph.Cmm.HaskellProgramCoverage' },
    'StgCmmLayout' => { name => 'SharedTermGraph.Cmm.Layout' },
    'StgCmmMonad' => { name => 'SharedTermGraph.Cmm.Monad' },
    'StgCmmPrim' => { name => 'SharedTermGraph.Cmm.Primitives' },
    'StgCmmProf' => { name => 'SharedTermGraph.Cmm.Profiling' },
    'StgCmmTicky' => { name => 'SharedTermGraph.Cmm.Ticky' },
    'StgCmmUtils' => { name => 'SharedTermGraph.Cmm.Utilities' },
    'StgCse' => { name => 'SharedTermGraph.CommonSubexpressionElimination' },
    'StgLint' => { name => 'SharedTermGraph.Lint' },
    'StgStats' => { name => 'SharedTermGraph.Statistics' },
    'StgSyn' => { name => 'SharedTermGraph.Syntax' },
    'Stream' => { name => 'Data.Stream' },
    'StringBuffer' => { name => 'Data.StringBuffer' },
    'SysTools' => { name => 'SystemTools' },
    'SysTools.BaseDir' => { name => 'SystemTools.BaseDirectory' },
    'SysTools.ExtraObj' => { name => 'SystemTools.ExtraObject' },
    'SysTools.Info' => { name => 'SystemTools.Info' },
    'SysTools.Process' => { name => 'SystemTools.Process' },
    'SysTools.Tasks' => { name => 'SystemTools.Tasks' },
    'SysTools.Terminal' => { name => 'SystemTools.Terminal' },
    'THNames' => { name => 'TemplateHaskell.Names' },
    'TargetReg' => { name => 'TargetRegisters' },
    'TcAnnotations' => { name => 'Typechecker.Annotations' },
    'TcArrows' => { name => 'Typechecker.Arrows' },
    'TcBackpack' => { name => 'Typechecker.Backpack' },
    'TcBinds' => { name => 'Typechecker.Bindings' },
    'TcCanonical' => { name => 'Typechecker.Canonicalizer' },
    'TcClassDcl' => { name => 'Typechecker.ClassDeclarations' },
    'TcDefaults' => { name => 'Typechecker.Defaults' },
    'TcDeriv' => {
	name => 'Typechecker.Deriving',
	doc => 'Handles @deriving@ clauses on @data@ declarations.'
    },
    'TcDerivInfer' => {
	name => 'Typechecker.Deriving.Inference',
	doc => 'Functions for inferring (and simplifying) the context for derived instances.'
    },
    'TcDerivUtils' => {
	name => 'Typechecker.Deriving.Utilities',
	doc => 'Error-checking and other utilities for @deriving@ clauses or declarations.'
    },
    'TcEnv' => { name => 'Typechecker.Environment' },
    'TcErrors' => { name => 'Typechecker.Errors' },
    'TcEvTerm' => { name => 'Typechecker.Evidence.Term' },
    'TcEvidence' => { name => 'Typechecker.Evidence' },
    'TcExpr' => { name => 'Typechecker.Expression' },
    'TcFlatten' => { name => 'Typechecker.Flatten' },
    'TcForeign' => { name => 'Typechecker.ForeignDeclaration' },
    'TcGenDeriv' => {
	name => 'Typechecker.Generating.Derived',
	doc => 'Generating derived instance declarations
--
-- This module is nominally /subordinate/ to "GhcUser.Typechecker.Deriving", which is the /official/ interface to deriving-related things.
--
-- This is where we do all the grimy bindings\' generation.'
    },
    'TcGenFunctor' => {
	name => 'Typechecker.Generating.Functor',
	doc => 'The deriving code for the `Functor`, `Foldable`, and `Traversable` classes (equivalent to the code in "GhcUser.Typechecker.Generating.Derived", for other classes).'
    },
    'TcGenGenerics' => {
	name => 'Typechecker.Generating.Generics',
	doc => 'The deriving code for the `Generic` class (equivalent to the code in "GhcUser.Typechecker.Generating.Derived", for other classes)'
    },
    'TcHoleErrors' => { name => 'Typechecker.HoleErrors' },
    'TcHsSyn' => {
	name => 'Typechecker.Haskell.Syntax',
	doc => 'Specialisations of the "GhcUser.Haskell.Syntax" syntax for the typechecker.
--
-- This module is an extension of "GhcUser.Haskell.Syntax" syntax, for use in the type checker.',
    },
    'TcHsType' => { name => 'Typechecker.Haskell.Type' },
    'TcIface' => { name => 'Typechecker.Interface' },
    'TcInstDcls' => { name => 'Typechecker.InstanceDeclarations' },
    'TcInteract' => { name => 'Typechecker.InteractionSolver' },
    'TcMType' => { name => 'Typechecker.Monad' },
    'TcMatches' => { name => 'Typechecker.Matches' },
    'TcPat' => { name => 'Typechecker.Pattern' },
    'TcPatSyn' => { name => 'Typechecker.Pattern.Synonyms' },
    'TcPluginM' => { name => 'Typechecker.Plugin.Monad' },
    'TcRnDriver' => {
	name => 'Typechecker.Renaming.Driver',
	doc => '__Typechecking a whole module__
--
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeChecker'
    },
    'TcRnExports' => { name => 'Typechecker.Renaming.Exports' },
    'TcRnMonad' => {
	name => 'Typechecker.Renaming.Monad',
	doc => 'Functions for working with the typechecker environment (setters, getters...).'
    },
    'TcRnTypes' => {
	name => 'Typechecker.Renaming.Types',
	doc => 'Various types used during typechecking, please see "GhcUser.Typechecker.Renaming.Monad" as well for operations on these types. You probably want to import it, instead of this module.
--
-- All the monads exported here are built on top of the same `IOEnv` monad. The monad functions like a `Reader` monad in the way it passes the environment around. This is done to allow the environment to be manipulated in a stack like fashion when entering expressions... etc.
--
-- For state that is global and should be returned at the end (e.g not part of the stack mechanism), you should use a `TcRef` (= `IORef`) to store them.'
    },
    'TcRules' => { name => 'Typechecker.Rules' },
    'TcSMonad' => {
	name => 'Typechecker.ConstraintSolver',
	doc => 'Type definitions for the constraint solver.',
	hidden => ['isFilledMetaTyVar'],
	rename => [
	    { old => 'isFilledMetaTyVar_maybe', new => 'isFilledMetaTyVar' }
	    ]
    },
    'TcSigs' => { name => 'Typechecker.Signatures' },
    'TcSimplify' => { name => 'Typechecker.Simplify' },
    'TcSplice' => { name => 'Typechecker.Splice' },
    'TcTyClsDecls' => { name => 'Typechecker.Type.Class.Declarations' },
    'TcTyDecls' => { name => 'Typechecker.Type.Declarations' },
    'TcType' => { name => 'Typechecker.Type' },
    'TcTypeNats' => { name => 'Typechecker.TypeLevelLiterals' },
    'TcTypeable' => { name => 'Typechecker.Typeable' },
    'TcUnify' => { name => 'Typechecker.Unify' },
    'TcValidity' => { name => 'Typechecker.Validity' },
    'TidyPgm' => { name => 'TidyProgram' },
    'TmOracle' => { name => 'TermEqualityOracle' },
    'ToIface' => { name => 'ToInterface' },
    'TrieMap' => { name => 'Data.TrieMap' },
    'TyCoRep' => { name => 'Type.Coercion.Representation' },
    'TyCon' => { name => 'Type.Constructor' },
    'Type' => {
	name => 'Type',
	doc => 'Main functions for manipulating types and type-related things.',
	hidden => [
	    'getClassPredTys',
	    'getEqPredTys',
	    'getRuntimeRep',
	    'getTyVar',
	    'isCoercionTy',
	    'isIPPred',
	    'splitAppTy',
	    'splitFunTy',
	    'splitPiTy',
	    'splitTyConApp',
	    'tyConAppArgs',
	    'tyConAppTyCon'
	    ],
	rename => [
	    { old => 'binderRelevantType_maybe', new => 'binderRelevantType',
	      doc => 'Extract a relevant type, if there is one.'
	    },
	    { old => 'getCastedTyVar_maybe', new => 'getCastedTyVar'} ,
	    { old => 'getClassPredTys_maybe', new => 'getClassPredTys' },
	    { old => 'getEqPredTys_maybe', new => 'getEqPredTys' },
	    { old => 'getRuntimeRepFromKind_maybe', new => 'getRuntimeRepFromKind' },
	    { old => 'getRuntimeRep_maybe', new => 'getRuntimeRep' },
	    { old => 'getTyVar_maybe', new => 'getTyVar' },
	    { old => 'isCoercionTy_maybe', new => 'isCoercionTy' },
	    { old => 'isIPPred_maybe', new => 'isIPPred' },
	    { old => 'isLiftedType_maybe', new => 'isLiftedType' },
	    { old => 'repGetTyVar_maybe', new => 'repGetTyVar' },
	    { old => 'repSplitAppTy_maybe', new => 'repSplitAppTy' },
	    { old => 'repSplitTyConApp_maybe', new => 'repSplitTyConApp' },
	    { old => 'splitAppTy_maybe', new => 'splitAppTy' },
	    { old => 'splitCastTy_maybe', new => 'splitCastTy' },
	    { old => 'splitCoercionType_maybe', new => 'splitCoercionType' },
	    { old => 'splitForAllTy_maybe', new => 'splitForAllTy' },
	    { old => 'splitFunTy_maybe', new => 'splitFunTy' },
	    { old => 'splitListTyConApp_maybe', new => 'splitListTyConApp' },
	    { old => 'splitPiTy_maybe', new => 'splitPiTy' },
	    { old => 'splitTyConApp_maybe', new => 'splitTyConApp' },
	    { old => 'tcRepSplitAppTy_maybe', new => 'tcRepSplitAppTy' },
	    { old => 'tcRepSplitTyConApp_maybe', new => 'tcRepSplitTyConApp' },
	    { old => 'tcSplitTyConApp_maybe', new => 'tcSplitTyConApp' },
	    { old => 'tyBinderVar_maybe', new => 'tyBinderVar' },
	    { old => 'tyConAppArgs_maybe', new => 'tyConAppArgs' },
	    { old => 'tyConAppTyConPicky_maybe', new => 'tyConAppTyConPicky' },
	    { old => 'tyConAppTyCon_maybe', new => 'tyConAppTyCon' },
	    { old => 'userTypeError_maybe', new => 'userTypeError' },
	    ]
    },
    'TysPrim' => { name => 'Type.Primitive' },
    'TysWiredIn' => { name => 'Type.WiredIn' },
    'UnVarGraph' => { name => 'Data.Graph.Undirected.Variable' },
    'UnariseStg' => { name => 'SharedTermGraph.Unarise' },
    'Unify' => { name => 'Unify' },
    'UniqDFM' => { name => 'Unique.Map.Finite.Deterministic' },
    'UniqDSet' => { name => 'Unique.Set.Deterministic' },
    'UniqFM' => { name => 'Unique.Map.Finite' },
    'UniqMap' => { name => 'Unique.Map' },
    'UniqSet' => {
	name => 'Unique.Set',
	doc => 'Specialised sets, for things with `GhcUser.Unique.Unique`s.
--
-- Based on "GhcUser.Unique.Map.Finite" (as you would expect).
--
-- Basically, the things need to be in class `GhcUser.Unique.Uniquable`.
',
    },
    'UniqSupply' => { name => 'Unique.Supply' },
    'Unique' => {
	name => 'Unique',
	doc => '`GhcUser.Unique.Unique`s are used to distinguish entities in the compiler (`Id`s, `Class`es, etc.) from each other. Thus, `GhcUser.Unique.Unique`s are the basic comparison key in the compiler.
--
-- If there is any single operation that needs to be fast, it is `GhcUser.Unique.Unique` comparison. Unsurprisingly, there is quite a bit of huff-and-puff directed to that end.
--
-- Some of the other hair in this code is to be able to use a splittable `GhcUser.Unique.Supply.UniqSupply` if requested/possible (not standard Haskell).',
    },
    'Util' => { name => 'Utilities' },
    'Var' => { name => 'Variable' },
    'VarEnv' => { name => 'Variable.Environment' },
    'VarSet' => { name => 'Variable.Set' },
    'WorkWrap' => { name => 'WorkerWrapper' },
    'WwLib' => { name => 'WorkerWrapper.Library' },
    'X86.CodeGen' => { name => 'Platform.X86.CodeGen.CodeGen' },
    'X86.Cond' => { name => 'Platform.X86.Condition' },
    'X86.Instr' => { name => 'Platform.X86.Instructions' },
    'X86.Ppr' => { name => 'Platform.X86.PrettyPrint' },
    'X86.RegInfo' => { name => 'Platform.X86.Registers.Info' },
    'X86.Regs' => { name => 'Platform.X86.Registers' },
);

my $tt = Template->new({
    INCLUDE_PATH => './templates',
}) || die "$Template::ERROR\n";

my $script = basename($0);

my $warning = "-- __WARNING__: This file is automatically generated by ${script}.
-- It is not managed by version control. Any changes you make to it will be lost.";

my $cabal_vars = {
    warning => $warning,
    modules => [sort (map { $_->{name} } values %modules)]
};

$tt->process('ghc-user.cabal.template', $cabal_vars, './ghc-user.cabal')
    || die $tt->error(), "\n";

while (my ($old_module_name, $new_module) = each %modules)
{
    my $vars = {
	warning => $warning,
	old => $old_module_name,
	new => $new_module->{name},
	doc => $new_module->{doc},
	hidden => $new_module->{hidden},
	rename => $new_module->{rename}
    };

    $new_module->{name} =~ s/\./\//g;
    my $output = "./src/GhcUser/" . $new_module->{name} . ".hs";
 
    $tt->process('Module.hs.template', $vars, $output)
	|| die $tt->error(), "\n";
}
