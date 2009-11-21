import Expression;

enum Value {
    VNative(body: Value -> Value);
    VLambda(environment: Hash<Value>, pattern: String, body: Expression);
    VObject(parent: Null<Value>, fields: Hash<Value>);
    VString(value: String);
    VInteger(value: Int);
    VFloat(value: Float);
    VBoolean(value: Bool);
}

class Values {
    public static function generateEmptyList() {
        var fields = new Hash<Value>();
        fields.set("after", VNative(function(other) { 
            return getCons(other, emptyList);
        }));
        fields.set("empty", VNative(function(other) { 
            return VBoolean(true);
        }));
        return VObject(null, fields);
    }

    private static var emptyList = generateEmptyList();
    private static var nothing = new Hash<Value>();
    private static var voidObject = VObject(null, nothing);

    public static function getObject(object: Value): Value {
        switch(object) {
            case VObject(parent, fields):
                return object;
            case VLambda(environment, pattern, body):
                var fields = new Hash<Value>();
                fields.set("get", object);
                fields.set("string", VLambda(nothing, "_", EString("{ ... }")));
                return VObject(null, fields);
            case VNative(body):
                var fields = new Hash<Value>();
                fields.set("get", object);
                fields.set("string", VLambda(nothing, "_", EString("{ ... }")));
                return VObject(null, fields);
            case VBoolean(value):
                var fields = new Hash<Value>();
                fields.set("then", VLambda(nothing, "t",
                    if(value) ECall(EVariable("t"), Expressions.getVoid())
                    else Expressions.getVoid()
                ));
                fields.set("else", VLambda(nothing, "e",
                    if(!value) ECall(EVariable("e"), Expressions.getVoid())
                    else Expressions.getVoid()
                ));
                fields.set("thenElse", VLambda(nothing, "t", ELambda("e",
                    if(value) ECall(EVariable("t"), Expressions.getVoid())
                    else ECall(EVariable("e"), Expressions.getVoid())
                )));
                fields.set("string", VLambda(nothing, "_", EString(
                    Std.string(value)
                )));
                fields.set("not", VNative(function(_) { return VBoolean(!value); }));
                return VObject(null, fields);
            case VInteger(value):
                var fields = new Hash<Value>();
                fields.set("<", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value < other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set(">", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value > other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("<=", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value <= other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set(">=", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value >= other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("==", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value == other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("!=", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value != other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("+", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VInteger(value + other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("-", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VInteger(value - other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("*", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VInteger(value * other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("/", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VInteger(Math.floor(value / other));
                    default:
                        throw "runtime type error";
                }}));
                fields.set("%", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VInteger(value % other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("^", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VInteger(Math.round(Math.pow(value, other)));
                    default:
                        throw "runtime type error";
                }}));
                fields.set("negated", VLambda(nothing, "_", EInteger(
                    -value
                )));
                fields.set("sign", VLambda(nothing, "_", EInteger(
                    if(value > 0) 1 else if(value < 0) -1 else 0
                )));
                fields.set("float", VLambda(nothing, "_", EFloat(
                    value
                )));
                fields.set("string", VLambda(nothing, "_", EString(
                    Std.string(value)
                )));
                return VObject(null, fields);
            case VFloat(value):
                var fields = new Hash<Value>();
                fields.set("<", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VBoolean(value < other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set(">", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VBoolean(value > other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("<=", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VBoolean(value <= other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set(">=", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VBoolean(value >= other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("==", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VBoolean(value == other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("!=", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VBoolean(value != other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("+", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VFloat(value + other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("-", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VFloat(value - other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("*", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VFloat(value * other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("/", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VFloat(Math.floor(value / other));
                    default:
                        throw "runtime type error";
                }}));
                fields.set("^", VNative(function(other) { switch(other) {
                    case VFloat(other):
                        return VFloat(Math.pow(value, other));
                    default:
                        throw "runtime type error";
                }}));
                fields.set("negated", VLambda(nothing, "_", EFloat(
                    -value
                )));
                fields.set("sign", VLambda(nothing, "_", EFloat(
                    if(value > 0) 1 else if(value < 0) -1 else 0
                )));
                fields.set("float", VLambda(nothing, "_", EFloat(
                    value
                )));
                fields.set("string", VLambda(nothing, "_", EString(
                    Std.string(value)
                )));
                return VObject(null, fields);
            case VString(value):
                var fields = new Hash<Value>();
                fields.set("string", VLambda(nothing, "_", EString(
                    value
                )));
                return VObject(null, fields);
            default:
                throw "Not implemented";
        }
    }

    public static inline function getEmpty(): Value {
        return emptyList;
    }

    public static function getCons(head, tail): Value {
        var list = null;
        var fields = new Hash<Value>();
        fields.set("head", VNative(function(other) { 
            return head;
        }));
        fields.set("tail", VNative(function(other) { 
            return tail;
        }));
        fields.set("after", VNative(function(other) { 
            return getCons(other, list);
        }));
        fields.set("empty", VNative(function(other) { 
            return VBoolean(false);
        }));
        list = VObject(null, fields);
        return list;
    }
    
    public static inline function getVoid(): Value {
        return voidObject;
    }
}

