"""
BITWISE
SPI - Simple Pascal Interpreter - Part 8
https://ruslanspivak.com/lsbasi-part8/
with additional modifications by Brian Borowski
1) Slightly more specific error messages
2) Handles floats as well as ints
3) Allows the user to type 'exit' to end the program
"""

###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################

# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
INTEGER, FLOAT, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF, CARRET, AMPERSAND, PIPE, TILDA, LEFTSHIFT, RIGHTSHIFT = (
    'INTEGER', 'FLOAT', 'PLUS', 'MINUS', 'MUL', 'DIV', '(', ')', 'EOF', 'CARRET', 'AMPERSAND', 'PIPE', 'TILDA', 'LEFTSHIFT', 'RIGHTSHIFT'
)


class Token(object):
    def __init__(self, token_type, value):
        self.type = token_type
        self.value = value

    def __str__(self):
        """String representation of the class instance.

        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
            Token(MUL, '*')
        """
        return 'Token({type}, {value})'.format(
            type=self.type,
            value=repr(self.value)
        )

    def __repr__(self):
        return self.__str__()


class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self, msg=None):
        if msg != None:
            raise Exception(msg)
        if self.pos > len(self.text) - 1:
            raise Exception('Unexpected end of file at position %d.' %
                            self.pos)
        raise Exception('Unexpected character \'%c\' at position %d.' %
                        (self.text[self.pos], self.pos))

    def advance(self):
        """Advance the `pos` pointer and set the `current_char` variable."""
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def number(self):
        """Return a (multidigit) integer or float consumed from the input."""
        is_int = True
        result = ''
        starting_pos = self.pos
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        if self.current_char == '.':
            is_int = False
            result += self.current_char
            self.advance()
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        if result == '.':
            self.error('Expected numeric value at position {pos:d}, received \'.\''.format(pos=starting_pos))
        return Token(INTEGER, int(result)) if is_int else Token(FLOAT, float(result))

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:

            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char.isdigit() or self.current_char == '.':
                return self.number()

            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')
            
            if self.current_char == '^':
                self.advance()
                return Token(CARRET, '^')
            
            if self.current_char == '&':
                self.advance()
                return Token(AMPERSAND, '&')
            
            if self.current_char == '|':
                self.advance()
                return Token(PIPE, '|')
            
            if self.current_char == '~':
                self.advance()
                return Token(TILDA, '~')

            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')

            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')

            if self.current_char == '/':
                self.advance()
                return Token(DIV, '/')

            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')

            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')

            if self.current_char == '<':
                self.advance()
                if self.current_char == '<':
                    self.advance()
                    return Token(LEFTSHIFT, '<<')
            
            if self.current_char == '>':
                self.advance()
                if self.current_char == '>':
                    self.advance()
                    return Token(RIGHTSHIFT, '>>')

            self.error()

        return Token(EOF, None)


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################

class AST(object):
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax.')

    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        """factor : (PLUS | MINUS) factor | INTEGER | FLOAT | LPAREN expr RPAREN"""
        token = self.current_token
        if token.type == PLUS or token.type == MINUS or token.type == TILDA: #or token.type == CARRET or token.type == AMPERSAND:
            self.eat(token.type)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == INTEGER:
            self.eat(INTEGER)
            return Num(token)
        elif token.type == FLOAT:
            self.eat(FLOAT)
            return Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node

    def term(self):
        """term : factor ((MUL | DIV) factor)*"""
        node = self.factor()

        while self.current_token.type in (MUL, DIV):
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            elif token.type == DIV:
                self.eat(DIV)
            node = BinOp(left=node, op=token, right=self.factor())

        return node
    #lower priority calls the higher priority

    def expr(self):
        """
        expr   : term ((PLUS | MINUS) term)*
        term   : factor ((MUL | DIV) factor)*
        factor : (PLUS | MINUS) factor | INTEGER | LPAREN expr RPAREN
        """
        node = self.term()

        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)
            elif token.type == MINUS:
                self.eat(MINUS)
            

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def shift(self):
        node = self.expr()
        while self.current_token.type in (LEFTSHIFT, RIGHTSHIFT):
            token = self.current_token
            if token.type == LEFTSHIFT:
                self.eat(LEFTSHIFT)
            elif token.type == RIGHTSHIFT:
                self.eat(RIGHTSHIFT)
            node = BinOp(left=node, op=token, right=self.expr())
        return node

    def bitwise_AND(self):
        node = self.shift()
        while self.current_token.type == AMPERSAND:
            token = self.current_token
            self.eat(AMPERSAND)
            node = BinOp(left=node, op=token, right=self.shift())
        return node   

    def bitwise_XOR(self):
        node = self.bitwise_AND()
        while self.current_token.type == CARRET:
            token = self.current_token
            self.eat(CARRET)
            node = BinOp(left=node, op=token, right=self.bitwise_AND())
        return node

    def bitwise_OR(self):
        node = self.bitwise_XOR()
        while self.current_token.type == PIPE:
            token = self.current_token
            self.eat(PIPE)
            node = BinOp(left=node, op=token, right=self.bitwise_XOR())
        return node

    def parse(self):
        node = self.bitwise_OR()
        if self.current_token.type != EOF:
            self.error()
        return node


###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

class NodeVisitor(object):
    def visit(self, node):
        if type(node).__name__ == 'NoneType':
            raise Exception('Unexpected end of input.')
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == CARRET:
            return self.visit(node.left) ^ self.visit(node.right)
        elif node.op.type == AMPERSAND:
            return self.visit(node.left) & self.visit(node.right)
        elif node.op.type == PIPE:
            return self.visit(node.left) | self.visit(node.right)
        elif node.op.type == LEFTSHIFT:
            return self.visit(node.left) << self.visit(node.right)
        elif node.op.type == RIGHTSHIFT:
            return self.visit(node.left) >> self.visit(node.right)
    def visit_Num(self, node):
        return node.value

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)
        elif op == TILDA:
            return ~self.visit(node.expr)

    def interpret(self):
        tree = self.parser.parse()
        if tree is None:
            return ''
        return self.visit(tree)


def main():
    while True:
        try:
            text = input('spi> ')
            if text == 'exit':
                raise EOFError
        except EOFError:
            # If user types CTRL+D
            print('Bye.')
            break
        if not text:
            continue
        try:
            lexer = Lexer(text)
            parser = Parser(lexer)
            interpreter = Interpreter(parser)
            result = interpreter.interpret()
            print(result)
        except Exception as error:
            print('Error:', error)


if __name__ == '__main__':
    main()
