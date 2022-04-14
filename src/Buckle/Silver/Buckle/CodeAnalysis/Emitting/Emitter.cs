using System;
using System.Collections.Generic;
using System.Linq;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace Buckle.CodeAnalysis.Emitting {
    internal sealed class Emitter {
        public DiagnosticQueue diagnostics = new DiagnosticQueue();
        private readonly Dictionary<FunctionSymbol, MethodDefinition> methods_ =
            new Dictionary<FunctionSymbol, MethodDefinition>();
        private readonly AssemblyDefinition assemblyDefinition_;
        private readonly Dictionary<TypeSymbol, TypeReference> knownTypes_;
        private TypeDefinition typeDefinition_;
        private readonly MethodReference consoleWriteLineReference_;

        private Emitter(string moduleName, string[] references) {
            var assemblies = new List<AssemblyDefinition>();

            if (diagnostics.Any())
                return;

            foreach (var reference in references) {
                try {
                    var assembly = AssemblyDefinition.ReadAssembly(reference);
                    assemblies.Add(assembly);
                } catch (BadImageFormatException) {
                    diagnostics.Push(Error.InvalidReference(reference));
                }
            }

            if (diagnostics.Any())
                return;

            var builtinTypes = new List<(TypeSymbol type, string metadataName)>() {
                (TypeSymbol.Any, "System.Object"),
                (TypeSymbol.Bool, "System.Boolean"),
                (TypeSymbol.Int, "System.Int32"),
                (TypeSymbol.String, "System.String"),
                (TypeSymbol.Void, "System.Void"),
            };

            var assemblyName = new AssemblyNameDefinition(moduleName, new Version(0, 1));
            assemblyDefinition_ = AssemblyDefinition.CreateAssembly(assemblyName, moduleName, ModuleKind.Console);
            knownTypes_ = new Dictionary<TypeSymbol, TypeReference>();

            foreach (var (typeSymbol, metadataName) in builtinTypes) {
                var typeReference = ResolveType(typeSymbol.name, metadataName);
                knownTypes_.Add(typeSymbol, typeReference);
            }

            TypeReference ResolveType(string buckleName, string metadataName) {
                var foundTypes = assemblies.SelectMany(a => a.Modules)
                    .SelectMany(m => m.Types)
                    .Where(t => t.FullName == metadataName)
                    .ToArray();

                if (foundTypes.Length == 1) {
                    var typeReference = assemblyDefinition_.MainModule.ImportReference(foundTypes[0]);
                    return typeReference;
                } else if (foundTypes.Length == 0)
                    diagnostics.Push(Error.RequiredTypeNotFound(buckleName, metadataName));
                else
                    diagnostics.Push(Error.RequiredTypeAmbiguous(buckleName, metadataName, foundTypes));

                return null;
            }

            MethodReference ResolveMethod(string typeName, string methodName, string[] parameterTypeNames) {
                var foundTypes = assemblies.SelectMany(a => a.Modules)
                    .SelectMany(m => m.Types)
                    .Where(t => t.FullName == typeName)
                    .ToArray();

                if (foundTypes.Length == 1) {
                    var foundType = foundTypes[0];
                    var methods = foundType.Methods.Where(m => m.Name == methodName);

                    foreach (var method in methods) {
                        if (method.Parameters.Count != parameterTypeNames.Length)
                            continue;

                        var allParametersMatch = true;

                        for (int i=0; i<parameterTypeNames.Length; i++) {
                            if (method.Parameters[i].ParameterType.FullName != parameterTypeNames[i]) {
                                allParametersMatch = false;
                                break;
                            }
                        }

                        if (!allParametersMatch)
                            continue;

                        return assemblyDefinition_.MainModule.ImportReference(method);
                    }

                    diagnostics.Push(Error.RequiredMethodNotFound(typeName, methodName, parameterTypeNames));
                    return null;
                } else if (foundTypes.Length == 0)
                    diagnostics.Push(Error.RequiredTypeNotFound(null, typeName));
                else
                    diagnostics.Push(Error.RequiredTypeAmbiguous(null, typeName, foundTypes));

                return null;
            }

            consoleWriteLineReference_ = ResolveMethod("System.Console", "WriteLine", new [] {"System.String"});
        }

        public DiagnosticQueue Emit(BoundProgram program, string outputPath) {
            if (diagnostics.Any())
                return diagnostics;

            var objectType = knownTypes_[TypeSymbol.Any];
            typeDefinition_ = new TypeDefinition(
                "", "Program", TypeAttributes.Abstract | TypeAttributes.Sealed, objectType);
            assemblyDefinition_.MainModule.Types.Add(typeDefinition_);

            foreach (var functionWithBody in program.functions)
                EmitFunctionDeclaration(functionWithBody.Key);

            foreach (var functionWithBody in program.functions)
                EmitFunctionBody(functionWithBody.Key, functionWithBody.Value);

            if (program.mainFunction != null)
                assemblyDefinition_.EntryPoint = methods_[program.mainFunction];

            assemblyDefinition_.Write(outputPath);

            return diagnostics;
        }

        private void EmitFunctionBody(FunctionSymbol function, BoundBlockStatement body) {
            var method = methods_[function];
            var ilProcessor = method.Body.GetILProcessor();

            foreach (var statement in body.statements)
                EmitStatement(ilProcessor, statement);
        }

        private void EmitStatement(ILProcessor ilProcessor, BoundStatement statement) {
            switch (statement.type) {
                case BoundNodeType.ExpressionStatement:
                    EmitExpressionStatement(ilProcessor, (BoundExpressionStatement)statement);
                    break;
                case BoundNodeType.VariableDeclarationStatement:
                    EmitVariableDeclarationStatement(ilProcessor, (BoundVariableDeclarationStatement)statement);
                    break;
                case BoundNodeType.GotoStatement:
                    EmitGotoStatement(ilProcessor, (BoundGotoStatement)statement);
                    break;
                case BoundNodeType.LabelStatement:
                    EmitLabelStatement(ilProcessor, (BoundLabelStatement)statement);
                    break;
                case BoundNodeType.ConditionalGotoStatement:
                    EmitConditionalGotoStatement(ilProcessor, (BoundConditionalGotoStatement)statement);
                    break;
                case BoundNodeType.ReturnStatement:
                    EmitReturnStatement(ilProcessor, (BoundReturnStatement)statement);
                    break;
                default:
                    diagnostics.Push(DiagnosticType.Fatal, $"unexpected node '{statement.type}'");
                    break;
            }
        }

        private void EmitReturnStatement(ILProcessor ilProcessor, BoundReturnStatement statement) {
            throw new NotImplementedException();
        }

        private void EmitConditionalGotoStatement(ILProcessor ilProcessor, BoundConditionalGotoStatement statement) {
            throw new NotImplementedException();
        }

        private void EmitLabelStatement(ILProcessor ilProcessor, BoundLabelStatement statement) {
            throw new NotImplementedException();
        }

        private void EmitGotoStatement(ILProcessor ilProcessor, BoundGotoStatement statement) {
            throw new NotImplementedException();
        }

        private void EmitVariableDeclarationStatement(ILProcessor ilProcessor, BoundVariableDeclarationStatement statement) {
            throw new NotImplementedException();
        }

        private void EmitExpressionStatement(ILProcessor ilProcessor, BoundExpressionStatement statement) {
            EmitExpression(ilProcessor, statement.expression);

            if (statement.expression.lType != TypeSymbol.Void)
                ilProcessor.Emit(OpCodes.Pop);
        }

        private void EmitExpression(ILProcessor ilProcessor, BoundExpression expression) {
            switch (expression.type) {
                case BoundNodeType.UnaryExpression:
                    EmitUnaryExpression(ilProcessor, (BoundUnaryExpression)expression);
                    break;
                case BoundNodeType.LiteralExpression:
                    EmitLiteralExpression(ilProcessor, (BoundLiteralExpression)expression);
                    break;
                case BoundNodeType.BinaryExpression:
                    EmitBinaryExpression(ilProcessor, (BoundBinaryExpression)expression);
                    break;
                case BoundNodeType.VariableExpression:
                    EmitVariableExpression(ilProcessor, (BoundVariableExpression)expression);
                    break;
                case BoundNodeType.AssignmentExpression:
                    EmitAssignmentExpression(ilProcessor, (BoundAssignmentExpression)expression);
                    break;
                case BoundNodeType.EmptyExpression:
                    EmitEmptyExpression(ilProcessor, (BoundEmptyExpression)expression);
                    break;
                case BoundNodeType.CallExpression:
                    EmitCallExpression(ilProcessor, (BoundCallExpression)expression);
                    break;
                case BoundNodeType.CastExpression:
                    EmitCastExpression(ilProcessor, (BoundCastExpression)expression);
                    break;
                default:
                    diagnostics.Push(DiagnosticType.Fatal, $"unexpected node '{expression.type}'");
                    break;
            }
        }

        private void EmitCastExpression(ILProcessor ilProcessor, BoundCastExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitCallExpression(ILProcessor ilProcessor, BoundCallExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitEmptyExpression(ILProcessor ilProcessor, BoundEmptyExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitAssignmentExpression(ILProcessor ilProcessor, BoundAssignmentExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitVariableExpression(ILProcessor ilProcessor, BoundVariableExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitBinaryExpression(ILProcessor ilProcessor, BoundBinaryExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitLiteralExpression(ILProcessor ilProcessor, BoundLiteralExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitUnaryExpression(ILProcessor ilProcessor, BoundUnaryExpression expression) {
            throw new NotImplementedException();
        }

        private void EmitFunctionDeclaration(FunctionSymbol function) {
            var voidType = knownTypes_[TypeSymbol.Void];
            var method = new MethodDefinition(function.name, MethodAttributes.Static | MethodAttributes.Private, voidType);
            typeDefinition_.Methods.Add(method);
            methods_.Add(function, method);
        }

        public static DiagnosticQueue Emit(
            BoundProgram program, string moduleName, string[] references, string outputPath) {
            DiagnosticQueue diagnostics = new DiagnosticQueue();
            diagnostics.Move(program.diagnostics);

            var emitter = new Emitter(moduleName, references);
            return emitter.Emit(program, outputPath);
        }
    }
}
