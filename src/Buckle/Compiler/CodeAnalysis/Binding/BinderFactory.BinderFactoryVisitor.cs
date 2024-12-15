using System;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

internal sealed partial class BinderFactory {
    internal sealed class BinderFactoryVisitor : SyntaxVisitor<Binder> {
        private int _position;
        private BelteSyntaxNode _memberDeclaration;
        private Symbol _member;
        private BinderFactory _factory;

        internal void Initialize(
            BinderFactory factory,
            int position,
            BelteSyntaxNode memberDeclaration,
            Symbol member) {
            _factory = factory;
            _position = position;
            _memberDeclaration = memberDeclaration;
            _member = member;
        }

        internal void Clear() {
            _factory = null;
            _position = 0;
            _memberDeclaration = null;
            _member = null;
        }

        private Compilation _compilation => _factory._compilation;

        private SyntaxTree _syntaxTree => _factory.syntaxTree;

        private ConcurrentCache<BinderCacheKey, Binder> _binderCache => _factory._binderCache;

        private EndBinder _endBinder => _factory._endBinder;

        private bool _inScript => _factory.inScript;

        internal static BinderCacheKey CreateBinderCacheKey(BelteSyntaxNode node, NodeUsage usage) {
            return new BinderCacheKey(node, usage);
        }

        internal override Binder DefaultVisit(SyntaxNode parent) {
            return ((BelteSyntaxNode)parent).parent.Accept(this);
        }

        internal override Binder Visit(SyntaxNode node) {
            return ((BelteSyntaxNode)node).Accept(this);
        }

        private Binder VisitCore(SyntaxNode node) {
            return ((BelteSyntaxNode)node).Accept(this);
        }

        internal override Binder VisitCompilationUnit(CompilationUnitSyntax node) {
            if (node != _syntaxTree.GetRoot())
                throw new ArgumentOutOfRangeException(nameof(node), "node is not apart of the tree");

            var key = new BinderCacheKey(node, _inScript ? NodeUsage.CompilationUnitScript : NodeUsage.Normal);

            if (!_binderCache.TryGetValue(key, out var result)) {
                result = _endBinder;

                if (_inScript) {
                    // TODO
                } else {
                    var globalNamespace = _compilation.globalNamespaceInternal;
                    result = new InContainerBinder(globalNamespace, result);
                }

                _binderCache.TryAdd(key, result);
            }

            return result;
        }

        internal Binder VisitTypeDeclarationCore(TypeDeclarationSyntax parent, NodeUsage extraInfo) {
            var key = CreateBinderCacheKey(parent, extraInfo);

            if (!_binderCache.TryGetValue(key, out var resultBinder)) {
                resultBinder = VisitCore(parent.parent);

                if (extraInfo != NodeUsage.Normal) {
                    var typeSymbol = ((NamespaceOrTypeSymbol)resultBinder.containingMember).GetSourceTypeMember(parent);

                    if (extraInfo == NodeUsage.NamedTypeBase) {
                        resultBinder = new WithClassTemplateParametersBinder(typeSymbol, resultBinder);
                    } else {
                        // TODO Even though we dont have this sugar, does this class add something necessary?
                        // resultBinder = new WithPrimaryConstructorParametersBinder(typeSymbol, resultBinder);

                        resultBinder = new InContainerBinder(typeSymbol, resultBinder);

                        if (parent.templateParameterList is not null)
                            resultBinder = new WithClassTemplateParametersBinder(typeSymbol, resultBinder);
                    }
                }

                _binderCache.TryAdd(key, resultBinder);
            }

            return resultBinder;
        }

        // TODO finish this class

    }
}
