using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Lowering;

internal static class InitializerRewriter {
    internal static ImmutableArray<BoundStatement> RewriteConstructor(
        ImmutableArray<BoundInitializer> boundInitializers) {
        var builder = ArrayBuilder<BoundStatement>.GetInstance();

        foreach (var initializer in boundInitializers)
            builder.Add(RewriteInitializer(initializer));

        return builder.ToImmutableAndFree();

        BoundStatement RewriteInitializer(BoundInitializer initializer) {
            return initializer.kind switch {
                BoundNodeKind.FieldEqualsValue => RewriteFieldInitializer(initializer as BoundFieldEqualsValue),
                _ => throw ExceptionUtilities.UnexpectedValue(initializer.kind),
            };
        }
    }

    private static BoundExpressionStatement RewriteFieldInitializer(BoundFieldEqualsValue fieldInitializer) {
        var field = fieldInitializer.field;
        var boundReciever = field.isStatic ? null : new BoundThisExpression(field.containingType);

        var boundStatement = new BoundExpressionStatement(
            new BoundAssignmentExpression(
                new BoundFieldAccessExpression(
                    boundReciever,
                    field,
                    field.type,
                    null
                ),
                fieldInitializer.value,
                field.type
            )
        );

        return boundStatement;
    }
}
