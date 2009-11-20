enum Expression {
    EPosition(file: String, line: Int, column: Int, expression: Expression);
    ELambda(pattern: String, body: Expression);
    EObject(parent: Null<Expression>, fields: Hash<Expression>);
    EField(object: Expression, field: String);
    ECall(lambda: Expression, argument: Expression);
    EVariable(name: String);
    ELet(name: String, value: Expression, body: Expression);
    ERecursive(bindings: Hash<Expression>, body: Expression);
    ESequence(left: Expression, right: Expression);
    EEmpty;
    EString(value: String);
    EInteger(value: Int);
    EFloat(value: Float);
}

enum Pattern {
    PVariable(name: String);
    PHeadTail(head: Pattern, tail: Pattern);
    PEmpty;
    PString(value: String);
    PInteger(value: Int);
    PFloat(value: Float);
}

class Expressions {
    private static var empty = EObject(null, new Hash<Expression>());

    public static function free(expression: Expression, ?variables: Hash<Void>): Hash<Void> {
        if(variables == null) variables = new Hash<Void>();
        switch(expression) {
            case EPosition(file, line, column, expression):
                free(expression, variables);
            case ELambda(pattern, body):
                var already = variables.exists(pattern);
                free(body, variables);
                if(!already) variables.remove(pattern);
            case EObject(parent, fields):
                if(parent != null) free(parent, variables);
                for(expression in fields) {
                    free(expression, variables);
                }
            case EField(object, field):
                free(object, variables);
            case ECall(lambda, argument):
                free(lambda, variables);
                free(argument, variables);
            case EVariable(name):
                variables.set(name, null);
            case ELet(name, value, body):
                free(value, variables);
                var already = variables.exists(name);
                free(body, variables);
                if(!already) variables.remove(name);
            case ERecursive(bindings, body):
                for(expression in bindings) {
                    free(expression, variables);
                }
                var already = new Hash<Void>();
                for(key in bindings.keys()) {
                    if(variables.exists(key)) already.set(key, null);
                }
                free(body, variables);
                for(key in bindings.keys()) {
                    if(!already.exists(key)) already.remove(key);
                }
            case ESequence(left, right):
                free(left, variables);
                free(right, variables);
            case EEmpty:
            case EString(value):
            case EInteger(value):
            case EFloat(value):
        }
        return variables;
    }

    public static inline function getVoid(): Expression {
        return empty;
    }
}

