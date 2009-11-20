import Expression;

class Parser {
    private var source: Array<Int>;
    private var position: Int;
    private var file: String;
    private var line: Int;
    private var column: Int;
    
    private static var lowerA = "a".charCodeAt(0);
    private static var lowerE = "e".charCodeAt(0);
    private static var lowerF = "f".charCodeAt(0);
    private static var lowerN = "n".charCodeAt(0);
    private static var lowerR = "r".charCodeAt(0);
    private static var lowerT = "t".charCodeAt(0);
    private static var lowerU = "u".charCodeAt(0);
    private static var lowerX = "x".charCodeAt(0);
    private static var lowerZ = "z".charCodeAt(0);
    private static var upperA = "A".charCodeAt(0);
    private static var upperE = "E".charCodeAt(0);
    private static var upperF = "F".charCodeAt(0);
    private static var upperU = "U".charCodeAt(0);
    private static var upperX = "X".charCodeAt(0);
    private static var upperZ = "Z".charCodeAt(0);
    private static var zero = "0".charCodeAt(0);
    private static var nine = "9".charCodeAt(0);
    private static var space = " ".charCodeAt(0);
    private static var carriage = "\r".charCodeAt(0);
    private static var lineFeed = "\n".charCodeAt(0);
    private static var backslash = "\\".charCodeAt(0);
    private static var underscore = "_".charCodeAt(0);
    private static var bang = "!".charCodeAt(0);
    private static var plus = "+".charCodeAt(0);
    private static var dash = "-".charCodeAt(0);
    private static var tilde = "~".charCodeAt(0);
    private static var comma = ",".charCodeAt(0);
    private static var semicolon = ";".charCodeAt(0);
    private static var dot = ".".charCodeAt(0);
    private static var colon = ":".charCodeAt(0);
    private static var equal = "=".charCodeAt(0);
    private static var slash = "/".charCodeAt(0);
    private static var pipe = "|".charCodeAt(0);
    private static var beginParenthesis = "(".charCodeAt(0);
    private static var endParenthesis = ")".charCodeAt(0);
    private static var beginBracket = "[".charCodeAt(0);
    private static var endBracket = "]".charCodeAt(0);
    private static var beginBrace = "{".charCodeAt(0);
    private static var endBrace = "}".charCodeAt(0);
    private static var singleQuote = "'".charCodeAt(0);
    private static var doubleQuote = "\"".charCodeAt(0);
    
    private static function getter(input: String) {
        return input.charAt(0).toLowerCase() + input.substr(1);
    }
    
    private static function setter(input: String) {
        return "set" + input;
    }
    
    private static function generateOperatorCharacters(): IntHash<Void> {
        var result = new IntHash<Void>();
        var c = "%/=&|+-^*<>$" + "?~\\:.!@";
        for(i in 0...c.length) {
            result.set(c.charCodeAt(i), null);
        }
        return result;
    }

    private static function generateSpecialOperators() {
        var result = new Hash<Void>();
        result.set(":", null);
        result.set(".", null);
        result.set("|", null);
        result.set("!", null);
        result.set("=", null);
        result.set(":=", null);
        result.set("+=", null);
        result.set("-=", null);
        result.set("*=", null);
        result.set(".=", null);
        return result;
    }
    
    private static function chars(input) {
        var output = new IntHash<Void>();
        for(i in 0...input.length) {
            output.set(input.charCodeAt(i), null);
        }
        return output;
    }
    
    private static var operatorCharacters = generateOperatorCharacters();
    private static var specialOperators = generateSpecialOperators();
    private static var normalPrecedences = [
        {last: chars("$\\"), leftAssociative: false, rightAssociative: true},
        {last: chars("|:"), leftAssociative: true, rightAssociative: false},
        {last: chars("&."), leftAssociative: true, rightAssociative: false},
        {last: chars("=><"), leftAssociative: false, rightAssociative: false},
        {last: chars("+-~"), leftAssociative: true, rightAssociative: false},
        {last: chars("*/%"), leftAssociative: true, rightAssociative: false},
        {last: chars("^!@"), leftAssociative: false, rightAssociative: true},
    ];
    
    public function new() {
    }
    
    public function parse(text: String, file = null, line = 1, column = 1): Expression {
        this.source = new Array<Int>();
        this.source.push(-1);
        for(i in 0...text.length) {
            this.source.push(text.charCodeAt(i));
        }
        this.source.push(-1);
        this.position = 1;
        this.file = file;
        this.line = line;
        this.column = column - 1;
        ignoreNewline();
        var result = attempt(parseSequence);
        parseEnd();
        return if(result != null) result else Expressions.getVoid();
    }
    
    // Parser rules
    
    private function parseSequence() {
        var left = null;
        do {
            var statement = parseDefinition();
            if(statement == null) statement = attempt(parseExpression);
            if(statement == null) break;
            if(left == null) left = statement
            else left = ESequence(left, statement);
        } while(separator(semicolon));
        if(left == null) {
            throw new ParserException("Field expected", file, line, column);
        }
        return left;
    }

    private function parseDefinition() {
        var recurse = function(expression: Expression): Expression {
            var bindings = null;
            while(true) {
                // TODO: Recursive objects
                switch(expression) {
                    case ELet(identifier, value, body):
                        switch(value) {
                            case ELambda(pattern, sequence):
                                if(bindings == null) bindings = new Hash<Expression>();
                                bindings.set(identifier, value);
                                expression = body;
                            case EObject(parent, field):
                                if(bindings == null) bindings = new Hash<Expression>();
                                bindings.set(identifier, value);
                                expression = body;
                            default:
                                break;
                        }
                    case ERecursive(functions, body):
                        if(bindings == null) bindings = new Hash<Expression>();
                        for(key in functions.keys()) {
                            bindings.set(key, functions.get(key));
                            // TODO: There may be duplicates, examine the consequences
                        }
                        expression = body;
                    default:
                        break;
                }
            }
            if(bindings == null) return expression;
            return ERecursive(bindings, expression);
        }
        var self = this;
        var identifier = attempt(function() {
            var i = self.parseIdentifier();
            self.required(equal);
            return i;
        });
        if(identifier == null) return null;
        var first = identifier.charCodeAt(0);
        if(first >= lowerA && first <= lowerZ) {
            var value = parseExpression();
            if(!separator(semicolon)) return ELet(identifier, value, EVariable(identifier));
            var sequence = attempt(parseSequence);
            var result = ELet(identifier, value, if(sequence != null) sequence else EVariable(identifier));
            return recurse(result);
        } else if(first >= upperA && first <= upperZ) {
            
        }
        throw "Not implemented";
    }

    private function parseExpression() {
        return parseBinaryOperators(normalPrecedences, 0, parseApply);
    }

    private function parseApply() {
        if(optional(dash)) return ECall(EField(parseAtom(), "negate"), Expressions.getVoid());
        if(optional(bang)) return ECall(EField(parseAtom(), "not"), Expressions.getVoid());
        var left = parseAtom();
        while(true) {
            if(optional(dot)) {
                if(peek() >= upperA && peek() <= upperZ) {
                    var identifier = parseIdentifier();
                    if(optional(equal)) {
                        left = ECall(EField(left, setter(identifier)), parseExpression());
                    } else {
                        left = ECall(EField(left, getter(identifier)), Expressions.getVoid());
                    }
                } else {
                    var right = attempt(parseIdentifier);
                    if(right == null) right = parseString();
                    left = EField(left, right);
                }
            } else {
                var arguments = attempt(parseArguments);
                if(arguments == null) {
                    var right = attempt(parseAtom);
                    if(right == null) break;
                    arguments = [right];
                }
                if(optional(equal)) {
                    left = EField(left, "set");
                    arguments.push(parseExpression());
                }
                for(argument in arguments) {
                    left = ECall(left, argument);
                }
            }
        }
        return left;
    }
    
    private function parseArguments() {
        required(beginParenthesis);
        var arguments = [];
        do {
            if(peek() == endParenthesis) break;
            arguments.push(parseExpression());
        } while(separator(comma));
        required(endParenthesis);
        return if(arguments.length == 0) [Expressions.getVoid()] else arguments;
    }
    
    private function parseAtom() {
        var object = attempt(parseObject);
        if(object != null) return object;
        var lambda = attempt(parseLambda);
        if(lambda != null) return lambda;
        var list = attempt(parseEmpty);
        if(list != null) return list;
        var tuple = attempt(parseTuple);
        if(tuple != null) return tuple;
        var string = attempt(parseString);
        if(string != null) return EString(string);
        var float = attempt(parseFloat);
        if(float != null) return EFloat(float);
        var integer = attempt(parseInteger);
        if(integer != null) return EInteger(integer);
        return parseVariable();
    }

    private function parseTuple() {
        required(beginParenthesis);
        if(optional(endParenthesis)) return Expressions.getVoid();
        var elements = [];
        elements.push(parseExpression());
        while(separator(comma)) {
            var right = attempt(parseExpression);
            if(right == null) break;
            elements.push(right);
        }
        var result = elements.pop();
        elements.reverse();
        for(element in elements) {
            var fields = new Hash<Expression>();
            fields.set("getFirst", ELet("_t", element, ELambda("_", EVariable("_t"))));
            fields.set("getSecond", ELet("_t", result, ELambda("_", EVariable("_t"))));
            result = EObject(null, fields);
        }
        required(endParenthesis);
        return result;
    }
    
    private function parseLambda() {
        required(beginBrace);
        var result = if(peek() == pipe) {
            var cases = [];
            var patterns = parsePatterns();
            while(patterns != null) {
                var body = attempt(parseSequence);
                if(body == null) body = Expressions.getVoid();
                cases.push({patterns: patterns, body: body});
                patterns = attempt(parsePatterns);
            }
            // TODO: Use all patterns
            ELambda(cases[0].patterns[0], cases[0].body);
        } else {
            var body = attempt(parseSequence);
            ELambda("_", if(body != null) body else Expressions.getVoid());
        }
        required(endBrace);
        return result;
    }
    
    private function parsePatterns() {
        required(pipe);
        var patterns = [];
        do {
            // TODO: Real patterns
            var pattern = attempt(parseIdentifier);
            if(pattern == null) break;
            var value = pattern.charCodeAt(0);
            if(!(value >= lowerA && value <= lowerZ)) {
                throw new ParserException("Pattern expected", file, line, column);
            }
            patterns.push(pattern);
        } while(separator(comma));
        required(pipe);
        return patterns;
    }

    private function parsePattern() {
        var identifier = attempt(parseIdentifier);
        if(identifier != null) return PVariable(identifier);
        var string = attempt(parseString);
        if(string != null) return PString(string);
        var integer = attempt(parseInteger);
        if(integer != null) return PInteger(integer);
        var float = attempt(parseFloat);
        if(float != null) return PFloat(float);
        return parseEmptyPattern();
    }
    
    private function parseEmptyPattern() {
        required(beginBracket);
        var items = [];
        do {
            var item = attempt(parsePattern);
            if(item == null) break;
            items.push(item);
        } while(separator(comma));
        var result = if(optional(colon)) parsePattern() else null;
        items.reverse();
        for(item in items) {
            result = PHeadTail(item, result);
        }
        required(endBracket);
        return result;
    }

    private function parseEmpty() {
        // TODO: List comprehensions
        required(beginBracket);
        var items = [];
        do {
            var item = attempt(parseExpression);
            if(item == null) break;
            items.push(item);
        } while(separator(comma));
        var result = if(optional(colon)) parseExpression() else EEmpty;
        items.reverse();
        for(item in items) {
            result = ECall(EField(result, "after"), item);
        }
        required(endBracket);
        return result;
    }

    private function parseObject() {
        required(beginParenthesis);
        var parent = null;
        if(optional(colon)) {
            parent = parseExpression();
            if(!separator(comma)) {
                required(endParenthesis);
                return EObject(parent, new Hash<Expression>());
            }
        }
        var fields = new Hash<Expression>();
        do {
            var field = peek() >= upperA && peek() <= upperZ;
            var identifier = attempt(parseIdentifier);
            if(identifier == null) identifier = attempt(parseString);
            if(identifier == null) break;
            required(colon);
            var body = parseExpression();
            if(field) {
                fields.set(getter(identifier), ELet("_t", body, ELambda("_", EVariable("_t"))));
            } else {
                fields.set(identifier, body);
            }
        } while(separator(comma));
        required(endParenthesis);
        return EObject(parent, fields);
    }

    private function parseVariable() {
        if((peek() < lowerA || peek() > lowerZ) && peek() != underscore) {
            throw new ParserException("This identifier must start with a lowercase letter", file, line, column);
        }
        return EVariable(parseIdentifier());
    }
    
    private function parseBinaryOperators(precedences: Array<Precedence>, index, parseLeaf) {
        if(index >= precedences.length) {
            return parseLeaf();
        } 
        var precedence = precedences[index];
        var left = parseBinaryOperators(precedences, index + 1, parseLeaf);
        var firstOperator = true;
        var self = this;
        while(true) {
            var operator = attempt(function() { return self.parseOperator(precedence.last); });
            if(operator == null) break;
            var right = 
                if(precedence.leftAssociative) parseBinaryOperators(precedences, index + 1, parseLeaf)
                else if(precedence.rightAssociative) parseBinaryOperators(precedences, index, parseLeaf)
                else if(firstOperator) parseBinaryOperators(precedences, index + 1, parseLeaf)
                else throw new ParserException("This operator is non-associative", file, line, column);
            firstOperator = false;
            left = ECall(EField(left, operator), right);
        }
        return left;
    }

    private inline function parseOperator(last: IntHash<Void>): String {
        var value = advance();
        if(!operatorCharacters.exists(value)) {
            throw new ParserException("Expected operator", file, line, column);
        }
        var result = new StringBuf();
        result.addChar(value);
        value = peek();
        while(operatorCharacters.exists(value)) {
            result.addChar(advance());
            value = peek();
        }
        var operator = result.toString();
        if(specialOperators.exists(operator)) {
            throw new ParserException("Unexpected special operator", file, line, column);
        }
        if(!last.exists(operator.charCodeAt(operator.length - 1))) {
            throw new ParserException("Unexpected operator character or precedence", file, line, column);
        }
        ignoreNewline();
        return operator;
    }
    
    // Token parsers
    
    private inline function parseIdentifier(): String {
        if(peek() == underscore) {
            advance();
            var value = peek();
            if(((value >= lowerA && value <= lowerZ) 
                || (value >= upperA && value <= upperZ)
                || (value >= zero && value <= nine))) {
                throw new ParserException("Unexpected alphanumeric after underscore", file, line, column);
            }
            return "_";
        } else {
            var value = advance();
            if(!((value >= lowerA && value <= lowerZ) || (value >= upperA && value <= upperZ))) {
                throw new ParserException("Expected variable", file, line, column);
            }
            var result = new StringBuf();
            result.addChar(value);
            value = peek();
            while((value >= lowerA && value <= lowerZ) 
                || (value >= upperA && value <= upperZ)
                || (value >= zero && value <= nine)) {
                result.addChar(advance());
                value = peek();
            }
            if(peek() == underscore) {
                throw new ParserException("Unexpected underscore", file, line, column);
            }
            ignore();
            return result.toString();
        }
    }
    
    private inline function parseString(): String {
        var result = new StringBuf();
        var value = advance();
        if(value == singleQuote) {
            value = advance();
            while(true) {
                if(value == singleQuote) {
                    if(peek() == singleQuote) {
                        advance();
                        result.addChar(singleQuote);
                    } else {
                        break;
                    }
                } else {
                    result.addChar(value);
                }
                value = advance();
            }
        } else if(value == doubleQuote) {
            value = advance();
            while(true) {
                if(value == doubleQuote) {
                    break;
                } else if(value == backslash) {
                    if(peek() == lowerR) {
                        advance();
                        result.add("\r");
                    } else if(peek() == lowerN) {
                        advance();
                        result.add("\n");
                    } else if(peek() == lowerT) {
                        advance();
                        result.add("\t");
                    } else if(peek() == lowerX) {
                        advance();
                        var code = new StringBuf();
                        code.add("0x");
                        code.addChar(advance());
                        code.addChar(advance());
                        result.addChar(Std.parseInt(code.toString()));
                    } else if(peek() == lowerU) {
                        advance();
                        var code = new StringBuf();
                        code.add("0x");
                        code.addChar(advance());
                        code.addChar(advance());
                        code.addChar(advance());
                        code.addChar(advance());
                        result.addChar(Std.parseInt(code.toString()));
                    } else if(peek() == upperU) {
                        advance();
                        var code = new StringBuf();
                        code.add("0x");
                        code.addChar(advance());
                        code.addChar(advance());
                        code.addChar(advance());
                        code.addChar(advance());
                        code.addChar(advance());
                        code.addChar(advance());
                        result.addChar(Std.parseInt(code.toString()));
                    } else if(peek() == doubleQuote) {
                        advance();
                        result.addChar(doubleQuote);
                    } else if(peek() == backslash) {
                        advance();
                        result.addChar(backslash);
                    } else {
                        throw new ParserException("Unknown escape sequence", file, line, column);
                    }
                } else {
                    result.addChar(value);
                }
                value = advance();
            }
        } else {
            throw new ParserException("Expected string", file, line, column);
        }
        return result.toString();
    }
    
    private inline function parseInteger(): Int {
        var value = advance();
        var first = value;
        if(!((value >= zero && value <= nine) || value == dash)) {
            throw new ParserException("Expected integer", file, line, column);
        }
        var result = new StringBuf();
        result.addChar(value);
        if(value != dash && peek() == lowerX) result.addChar(advance());
        value = peek();
        if(first == dash && !(value >= zero && value <= nine)) {
            throw new ParserException("Expected integer", file, line, column);
        }
        while((value >= zero && value <= nine)) {
            result.addChar(advance());
            value = peek();
        }
        if(value == underscore || (value >= lowerA && value <= lowerZ) || (value >= upperA && value <= upperZ)) {
            throw new ParserException("Unexpected " + String.fromCharCode(value), file, line, column);
        }
        ignore();
        return Std.parseInt(result.toString());
    }
    
    private inline function parseFloat(): Float {
        var value = advance();
        if(!((value >= zero && value <= nine) || value == dash)) {
            throw new ParserException("Expected integer", file, line, column);
        }
        var result = new StringBuf();
        result.addChar(value);
        value = peek();
        while((value >= zero && value <= nine)) {
            result.addChar(advance());
            value = peek();
        }
        if(peek() != dot) {
            throw new ParserException("Expected .", file, line, column);
        }
        result.addChar(advance());
        if(!(peek() >= zero && peek() <= nine)) {
            throw new ParserException("Expected decimals", file, line, column);
        }
        value = peek();
        while((value >= zero && value <= nine)) {
            result.addChar(advance());
            value = peek();
        }
        if(value == lowerE || value == upperE) {
            result.addChar(advance());
            if(peek() == plus || peek() == dash) result.addChar(advance());
            value = peek();
            while((value >= zero && value <= nine)) {
                result.addChar(advance());
                value = peek();
            }
        }
        if(value == underscore || (value >= lowerA && value <= lowerZ) || (value >= upperA && value <= upperZ)) {
            throw new ParserException("Unexpected " + String.fromCharCode(value), file, line, column);
        }
        ignore();
        return Std.parseFloat(result.toString());
    }
    
    private inline function parseEnd(): Void {
        if(advance() != -1) {
            throw new ParserException("Expected end of file", file, line, column);
        }
    }

    // Utility functions
    
    private inline function separator(value: Int): Bool {
        return optional(lineFeed) || optional(value);
    }

    private inline function ignore(): Void {
        while(peek() == space || peek() == carriage) {
            advance();
        }
    }
    
    private inline function ignoreNewline(): Void {
        while(peek() == space || peek() == carriage || peek() == lineFeed) {
            advance();
        }
    }
    
    private inline function advance(): Int {
        column += 1;
        if(source[position - 1] == lineFeed) {
            line += 1;
            column = 1;
        }
        return source[position++];
    }

    private inline function peek(): Int {
        return source[position];
    }
    
    private inline function optional(value: Int): Bool {
        if(peek() != value) {
            return false;
        } else {
            advance();
            if(value == endParenthesis || value == endBracket || value == endBrace) {
                ignore();
            } else {
                ignoreNewline();
            }
            return true;
        }
    }

    private inline function required(value: Int): Void {
        if(advance() != value) {
            throw new ParserException("Expected " + String.fromCharCode(value), file, line, column);
        }
        if(value == endParenthesis || value == endBracket || value == endBrace) {
            ignore();
        } else {
            ignoreNewline();
        }
    }

    private inline function attempt<T>(body: Void -> T): T {
        var oldPosition = position;
        var oldLine = line;
        var oldColumn = column;
        try {
            return body();
        } catch(e: ParserException) {
            position = oldPosition;
            line = oldLine;
            column = oldColumn;
            return null;
        }
    }
}

class ParserException {
    private var message: String;
    private var file: String;
    private var line: Int;
    private var column: Int;
    
    public function new(message: String, file: String, line: Int, column: Int) {
        this.message = message;
        this.file = file;
        this.line = line;
        this.column = column;
    }
    
    public function toString() {
        var f = if(file != null) " in " + file else "";
        var l = if(line != 0) " at line " + line else "";
        var c = if(column != 0) ", column " + column else "";
        return message + f + l + c;
    }
}

typedef Precedence = {last: IntHash<Void>, leftAssociative: Bool, rightAssociative: Bool}

