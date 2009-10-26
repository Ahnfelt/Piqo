import Interpreter;
import Expression;
import Value;
import Parser;

class Main {
    private static function testCall() {
        return ECall(
            ELambda("x", 
                ELet("y", 
                    EVariable("x"), 
                    ESequence(Expressions.getVoid(), EVariable("y")))),
            Expressions.getVoid()
        );
    }
        
    private static function testScope() {
        return ECall(
            ELet("x",
                EInteger(42),
                ELambda("_", EVariable("x"))),
            Expressions.getVoid()
        );
    }

    private static function testRecursive() {
        var bindings = new Hash<Expression>();
        bindings.set("foo", ELambda("x", EVariable("x")));
        bindings.set("bar", ELambda("y", ECall(EVariable("foo"), EInteger(7))));
        return ERecursive(bindings, ECall(EVariable("bar"), Expressions.getVoid()));
    }

    public static function main() {
        //trace(new Interpreter().evaluate(testRecursive()));
        var text = "
one = ('+': {|other| other})
        ";
        try {
            var e = new Parser().parse(text);
            trace(e);
            var variables = new Hash<Value>();
            var number = 0;
            variables.set("print", VNative(function (v) {
                switch(v) {
                    case VString(value):
                        trace(number++ + ":" + value);
                        return Values.getVoid();
                    default:
                        throw "wrong format";
                }
            }));
            var v = new Interpreter().evaluate(e, variables);
            trace(v);
        } catch(e: ParserException) {
            trace(e);
        }
    }
}

