import Expression;
import Value;

class Interpreter {
    private var file: String;
    private var line: Int;
    private var column: Int;
    
    public function new() {
        this.file = null;
        this.line = 0;
        this.column = 0;
    }
    
    private function error(message: String) {
        var f = if(file != null) " in " + file else "";
        var l = if(line != 0) " at line " + line else "";
        var c = if(column != 0) ", column " + column else "";
        return new InterpreterException(message + f + l + c);
    }

    public function evaluate(expression: Expression, ?variables: Hash<Value>): Value {
        if(variables == null) variables = new Hash<Value>();
        switch(expression) {
            case EPosition(file, line, column, expression):
                var oldFile = this.file;
                var oldLine = this.line;
                var oldColumn = this.column;
                this.file = file;
                this.line = line;
                this.column = column;
                var result = evaluate(expression, variables);
                this.file = oldFile;
                this.line = oldLine;
                this.column = oldColumn;
                return result;
            case ELambda(pattern, body):
                var free = Expressions.free(body);
                free.remove(pattern);
                var environment = new Hash<Value>();
                for(key in free.keys()) {
                    environment.set(key, variables.get(key));
                }
                return VLambda(environment, pattern, body);
            case EObject(parent, fields):
                var object = if(parent != null) evaluate(parent, variables) else null;
                var added = new Hash<Value>();
                for(key in fields.keys()) {
                    added.set(key, evaluate(fields.get(key), variables));
                }
                return VObject(object, added);
            case EField(object, field):
                return getField(evaluate(object, variables), field);
            case ECall(lambda, argument):
                return call(evaluate(lambda, variables), evaluate(argument, variables));
            case EVariable(name):
                if(!variables.exists(name)) throw error(name + " is not defined");
                return variables.get(name);
            case ELet(name, value, body):
                var binding = evaluate(value, variables);
                var already = variables.get(name);
                variables.set(name, binding);
                var result = evaluate(body, variables);
                if(already != null) {
                    variables.set(name, already);
                } else {
                    variables.remove(name);
                }
                return result;
            case ERecursive(bindings, body):
                var already = new Hash<Value>();
                for(key in bindings.keys()) {
                    if(variables.exists(key)) already.set(key, variables.get(key));
                }
                var values = new Hash<Value>();
                for(key in bindings.keys()) {
                    var value = evaluate(bindings.get(key), variables);
                    values.set(key, value);
                    variables.set(key, value);
                }
                for(lambda in values) {
                    switch(lambda) {
                        case VLambda(environment, pattern, expression):
                            for(key in values.keys()) {
                                if(environment.exists(key)) environment.set(key, values.get(key));
                            }
                        case VObject(parent, fields):
                            for(field in fields) {
                                switch(field) {
                                    case VLambda(environment, pattern, expression):
                                        for(key in values.keys()) {
                                            if(environment.exists(key)) environment.set(key, values.get(key));
                                        }
                                    default:
                                }
                            }
                        default:
                            throw error("Lambda expected in recursive definition");
                    }
                }
                var result = evaluate(body, variables);
                for(key in bindings.keys()) {
                    if(already.exists(key)) {
                        variables.set(key, already.get(key));
                    } else {
                        variables.remove(key);
                    }
                }
                return result;
            case ESequence(left, right):
                evaluate(left, variables);
                return evaluate(right, variables);
            case EList:
                return VList([]);
            case EString(value):
                return VString(value);
            case EInteger(value):
                return VInteger(value);
            case EFloat(value):
                return VFloat(value);
            case EBoolean(value):
                return VBoolean(value);
        }
    }
    
    private function getField(object: Value, field: String): Value {
        switch(object) {
            case VObject(parent, fields):
                if(fields.exists(field)) {
                    return fields.get(field);
                } else if(parent != null) {
                    return getField(parent, field);
                } else {
                    throw error(field + " is not a field of this object");
                }
            default:
                return getField(Values.getObject(object), field);
        }
    }
    
    private function call(object: Value, argument: Value): Value {
        switch(object) {
            case VLambda(environment, pattern, body):
                environment.set(pattern, argument);
                var result = evaluate(body, environment);
                environment.remove(pattern);
                return result;
            case VNative(body):
                return body(argument);
            default:
                return call(getField(object, "get"), argument);
        }
    }
}

class InterpreterException {
    private var message: String;
    
    public function new(message: String) {
        this.message = message;
    }
    
    public function toString() {
        return message;
    }
}

