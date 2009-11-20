import Expression;

enum Value {
    VNative(body: Value -> Value);
    VLambda(environment: Hash<Value>, pattern: String, body: Expression);
    VObject(parent: Null<Value>, fields: Hash<Value>);
    VList(value: Array<Value>);
    VString(value: String);
    VInteger(value: Int);
    VFloat(value: Float);
    VBoolean(value: Bool);
}

class Values {
    private static var nothing = new Hash<Value>();
    private static var empty = VObject(null, nothing);

    public static function getObject(object: Value): Value {
        switch(object) {
            case VObject(parent, fields):
                return object;
            case VLambda(environment, pattern, body):
                var fields = new Hash<Value>();
                fields.set("get", object);
                return VObject(null, fields);
            case VNative(body):
                var fields = new Hash<Value>();
                fields.set("get", object);
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
                fields.set("getNot", VNative(function(_) { return VBoolean(!value); }));
                return VObject(null, fields);
            case VInteger(value):
                var fields = new Hash<Value>();
                fields.set("less", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value < other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("greater", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value > other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("lessEqual", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value <= other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("greaterEqual", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value >= other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("equal", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value == other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("notEqual", VNative(function(other) { switch(other) {
                    case VInteger(other):
                        return VBoolean(value != other);
                    default:
                        throw "runtime type error";
                }}));
                fields.set("getSign", VLambda(nothing, "_", EInteger(
                    if(value > 0) 1 else if(value < 0) -1 else 0
                )));
                return VObject(null, fields);
            default:
                throw "Not implemented";
        }
    }
    
    public static inline function getVoid(): Value {
        return empty;
    }
}

