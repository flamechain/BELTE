using System.Threading;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

internal partial class SourceDataContainerSymbol {
    private sealed class SourceDataContainerWithInitializerSymbol : SourceDataContainerSymbol {
        private readonly EqualsValueClauseSyntax _initializer;
        private readonly Binder _initializerBinder;

        private ConstantValue _lazyConstantValue;

        internal SourceDataContainerWithInitializerSymbol(
            Symbol containingSymbol,
            Binder scopeBinder,
            TypeSyntax typeSyntax,
            SyntaxToken identifierToken,
            EqualsValueClauseSyntax initializer,
            Binder initializerBinder,
            DataContainerDeclarationKind declarationKind)
            : base(containingSymbol, scopeBinder, true, typeSyntax, identifierToken, declarationKind) {
            _initializer = initializer;
            _initializerBinder = initializerBinder;
        }

        internal override ConstantValue GetConstantValue(
            SyntaxNode node,
            DataContainerSymbol inProgress,
            BelteDiagnosticQueue diagnostics) {
            if (isConstExpr && inProgress == this) {
                // TODO Add this when confirm this can actually happen
                // Potentially `constexpr A = A;`?
                // diagnostics?.Add(ErrorCode.ERR_CircConstValue, node.GetLocation(), this);
                return null;
            }

            MakeConstantValue(inProgress, null);
            return _lazyConstantValue;
        }

        internal override BelteDiagnostic[] GetConstantValueDiagnostics(BoundExpression boundInitValue) {
            MakeConstantValue(null, boundInitValue);
            return _lazyConstantValue is null ? [] : _lazyConstantValue.diagnostics;
        }

        private protected override TypeWithAnnotations InferTypeOfImplicit(BelteDiagnosticQueue diagnostics) {
            var initializer = _initializerBinder.BindInferredDataContainerInitializer(diagnostics, refKind, _initializer, _initializer);
            return new TypeWithAnnotations(initializer.type);
        }

        private void MakeConstantValue(DataContainerSymbol inProgress, BoundExpression boundInitValue) {
            if (isConstExpr && _lazyConstantValue is null) {
                var diagnostics = BelteDiagnosticQueue.GetInstance();
                var type = this.type;

                if (boundInitValue is null) {
                    var inProgressBinder = new LocalInProgressBinder(this, _initializerBinder);
                    boundInitValue = inProgressBinder.BindDataContainerInitializerValue(
                        _initializer,
                        refKind,
                        type,
                        diagnostics
                    );
                }

                var value = ConstantValueHelpers.GetAndValidateConstantValue(
                    boundInitValue,
                    this,
                    type,
                    _initializer.value,
                    diagnostics
                );

                Interlocked.CompareExchange(
                    ref _lazyConstantValue,
                    new ConstantValue(value, diagnostics.ToArrayAndFree()),
                    null
                );
            }
        }
    }
}
