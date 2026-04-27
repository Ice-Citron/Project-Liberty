package a40009_imperial.structurespack01.linkedlist.sonsOfMars

class MadDash {
}

// 1. The Tokens
enum class TokenType {
    WHILE, IF, ELSE, LPAREN, RPAREN, LBRACE, RBRACE, IDENTIFIER, EOF
}

data class Token(val type: TokenType, val lexeme: String)

// 2. The AST Nodes
abstract class StmtNode
abstract class ExprNode

class WhileNode : StmtNode() {
    var condition: ExprNode? = null
    var body: StmtNode? = null
}

class IfNode : StmtNode() {
    var condition: ExprNode? = null
    var thenBranch: StmtNode? = null
    var elseBranch: StmtNode? = null
}

// Dummy nodes just so our helper methods have something to return
class IdentifierExpr(val name: String) : ExprNode()
class BlockStmt(val statements: List<StmtNode>) : StmtNode()

// 3. The Parser
class Parser(private val tokens: List<Token>) {
    private var currentIndex = 0

    fun peekNextToken(): Token? {
        if (currentIndex >= tokens.size) return null
        return tokens[currentIndex]
    }

    fun consumeToken(): Token {
        if (currentIndex >= tokens.size) throw IllegalStateException("Unexpected end of file")
        val token = tokens[currentIndex]
        currentIndex++ // Moves the bookmark forward!
        return token
    }

    fun consume(expectedType: TokenType): Token {
        val token = peekNextToken()
        if (token == null || token.type != expectedType) {
            throw IllegalArgumentException("Syntax Error: Expected $expectedType but got ${token?.type}")
        }
        return consumeToken()
    }

    // --- SUB-PARSERS (Pre-written for you) ---

    fun parseExpression(): ExprNode {
        // In reality, this parses math. Here, we just eat an identifier (like 'x')
        val token = consume(TokenType.IDENTIFIER)
        return IdentifierExpr(token.lexeme)
    }

    fun parseBlock(): StmtNode {
        consume(TokenType.LBRACE)
        // In reality, we would loop and parse statements here
        consume(TokenType.RBRACE)
        return BlockStmt(emptyList())
    }

    // --- YOUR EXERCISES ---

    fun parseWhileStatement(): WhileNode {
        // TODO: Consume WHILE, LPAREN, parseExpression, RPAREN, parseBlock.
        // TODO: Construct and return the WhileNode using with()

        return WhileNode() // Replace me
    }

    fun parseIfStatement(): IfNode {
        // TODO: Consume IF, LPAREN, parseExpression, RPAREN, parseBlock (for thenBranch).
        // TODO: Peek for ELSE. If it exists, consume it and parseBlock (for elseBranch).
        // TODO: Construct and return the IfNode using with()

        return IfNode() // Replace me
    }
}

// --- HOW IT ACTUALLY RUNS ---
fun main() {
    // 1. The Lexer creates the list of tokens from raw source code: "while ( x ) { }"
    val myCodeTokens = listOf(
        Token(TokenType.WHILE, "while"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.IDENTIFIER, "x"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.LBRACE, "{"),
        Token(TokenType.RBRACE, "}")
    )

    // 2. We hand the list to the Parser
    val myParser = Parser(myCodeTokens)

    // 3. We tell it to start parsing!
    val result = myParser.parseWhileStatement()
    println("Successfully parsed a while loop!")
}































