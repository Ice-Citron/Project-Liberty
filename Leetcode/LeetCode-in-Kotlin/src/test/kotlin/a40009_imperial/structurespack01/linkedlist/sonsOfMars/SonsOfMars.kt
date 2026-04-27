package a40009_imperial.structurespack01.linkedlist.sonsOfMars

import org.junit.jupiter.api.assertThrowsExactly
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith



sealed interface Action {
    class Scan(val target: String)
    class Exploit(val payload: String)
    object Sleep
}

fun Action.execute(): String = when(this) {
    is Action.Scan -> "Scanning [${target}]"
    is Action.Exploit -> "Deploying [$payload]"
    is Action.Sleep -> "Entering standby"
}




























/*
Exercise 2: The Stateful `enum class`
* */
enum class ThreatLevel(val priority: Int) {
    LOW(1),
    HIGH(5),
    SYSTEM_FAILURE(10);

    fun isUrgent(): Boolean = priority > 3
}


class ProgramExecutor (
    val threadBody: Stmt,
    val pauseMs: Long,
    val store: MutableMap<String, Int>,
    private val lock: ReentrantLock,
) : Runnable {
    override fun run() {
        var currentStatement: Stmt? = threadBody
        while (currentStatement != null) {
            Thread.sleep(pauseMs)
            lock.withLock {
                currentStatement = currentStatement?.step(store)
            }
        }
    }
}



















/*
fun parseIdentifier(): String? = peekNextToken()?.let {
    if (it.type == TokenType.IDENTIFIER) {
        consumeToken()
        it.lexeme
    } else {
        null
    }
}
 */


// assertSame   --> `===`
// assertEquals --> `==`





class SingleInterpreter {
    private val memory = mutableMapOf<String, Int>()

    fun assign(variable: String, value: Int) {
        memory[variable] = value
    }

    fun lookup(variable: String): Int {
        return memory[variable] ?:
        throw NoSuchElementException("Variable: $variable not found.")
    }

    fun divide(var1: String, var2: String): Int {
        val num1 = lookup(var1)
        val num2 = lookup(var2)
        if (num2 == 0) throw ArithmeticException("Division by zero")
        return num1 / num2
    }
}


class SingleInterpreterTests {
    private lateinit var interpreter: SingleInterpreter

    @BeforeTest
    fun setup() {
        interpreter = SingleInterpreter().apply {
            assign("x", 50)
            assign("x", 100)
            assign("y", 30)
            assign("c", 50)
            assign("d", 0)
        }
    }

    @Test
    fun `assign stores variable correctly`() {
        val interpreter = agashe_rucha.SingleInterpreter().apply{
            assign("ades", 10)
        }
        assertEquals(10, interpreter.lookup("ades"))
    }

    @Test
    fun `lookup throws NoSuchElementException for undefined variables`() {
        assertEquals(100, interpreter.lookup("x"))
        assertEquals(30, interpreter.lookup("y"))
        assertFailsWith<NoSuchElementException> {
            interpreter.lookup("a")
        }
        assertFailsWith<NoSuchElementException> {
            interpreter.lookup("b")
        }
    }

    @Test
    fun `Verify if illegal operation of division by zero throws exception`() {
        assertFailsWith<ArithmeticException> {
            interpreter.divide("c", "d")
        }
        assertFailsWith<ArithmeticException> {
            interpreter.divide("x", "d")
        }
        assertFailsWith<ArithmeticException> {
            interpreter.divide("y", "d")
        }
    }
}


// BAD CODE: Fix this class
class InterpreterTests {
    private lateinit var globalInterpreter: SingleInterpreter

    @BeforeTest
    fun setup() {
        globalInterpreter = SingleInterpreter()
    }

    @Test
    fun `test one`() {
        globalInterpreter.assign("count", 5)
        assertEquals(5, globalInterpreter.lookup("count"))
    }

    @Test
    fun `test two`() {
        assertFailsWith<NoSuchElementException> {
            globalInterpreter.lookup("count")
        }
    }
}








/*
    This snippet is a classic Kotlin fail-fast pattern utilising the ELVIS
    OPERATOR (`?:`). It attempts to fetch `variable` from `memory` using the
    bracket operator (`get()`). If the lookup fins a value, it returns it. If
    the lookup returns `null`, the Elvis operator catches the `null` and executes
    the right side, throwing the exception. It is essentially a concise way to
    say, "Give me this value, and if it..."

    ... tailored for MAP and HASHMAP... In kotlin, `map[key]` natively returns
    a nullable type V?... so a missing key gracefully returns `null`, perfectly
    triggering the Elvis operator... For index-based ADTs... asking for an
    out-of-bounds index ... array[99]... doesn't return null; it immediately
    crashes with an `IndexOutOfBoundsException`... before the Elvis
    operator even gets a chance to look at it. ... To use the safe fallback pattern

    return array.getOrNull(index) ?: throw NoSuchElementException("Element at index $index not found.")
* */






class sienarune {
}


class BankAccount1(
    var balance: Int = 0,
    val initialBalance: Int = 0,
) {
    fun withdraw(amount: Int) {
        check(amount < balance) {
            "Error! Cannot withdraw more than available balance."
        }
        balance -= amount
    }
}


class BankAccountTests {
    // lateinit lets use bypass nullable `BankAccount?` types.
    private lateinit var account: BankAccount1

    @BeforeTest
    fun setup() {
        account = BankAccount1(balance = 100)
    }

    @Test
    fun `withdrawing valid amount reduces balance correctly`() {
        account.withdraw(40)

        assertEquals(60, account.balance)   // Assert
    }

    @Test
    fun `withdrawing more than balance throws exception`() {
        // Act & Assert combined nicely into a Kotlin lambda
        assertFailsWith<IllegalStateException> {
            account.withdraw(500)
        }
    }
}




/*
// 1. The Token System
enum class TokenType {
    IDENTIFIER, VAR, EQUALS, SEMICOLON, PLUS, MINUS, NUMBER, EOF
}

data class Token(val type: TokenType, val lexeme: String) {
    // Helper for Exercise 3
    fun isOperator(): Boolean {
        return type == TokenType.PLUS || type == TokenType.MINUS
    }
}

// 2. The Abstract Syntax Tree (AST) Nodes
abstract class ExprNode

class VarDeclNode {
    var identifier: String = ""
    var initializer: ExprNode? = null
}

class BinaryExprNode : ExprNode() {
    var left: ExprNode? = null
    var operator: String = ""
    var right: ExprNode? = null
}

// 3. The Dummy Parser Shell (to satisfy the function calls)
class DummyParser {

    fun peekNextToken(): Token? = Token(TokenType.IDENTIFIER, "x")

    fun consumeToken(): Token = Token(TokenType.IDENTIFIER, "x")

    fun consume(expectedType: TokenType): Token = Token(expectedType, "dummy")

    fun parseExpression(): ExprNode = BinaryExprNode()

    fun parsePrimaryExpression(): ExprNode = BinaryExprNode()

    fun parseVariableDeclaration(): VarDeclNode {
        consume(TokenType.VAR)
        val nextToken = peekNextToken()
        val parsedinitialiser =
            if (nextToken != null && nextToken.type == TokenType.EQUALS) {
                consume(TokenType.EQUALS)
                parseExpression()
            } else {
                null
            }
        consume(TokenType.SEMICOLON)

        val declNode = VarDeclNode()
        return with(declNode) {
            initializer = parsedinitialiser
            identifier = consume(TokenType.IDENTIFIER).lexeme
            declNode
        }
    }

    fun parseBinaryExpression(leftNode: ExprNode): ExprNode {
        val nextToken = peekNextToken()
        var currentExpr = leftNode

        if (nextToken != null) {
            if (nextToken.isOperator()) {
                val operatorToken = consumeToken()
                val rightNode = parsePrimaryExpression()

                currentExpr = with (BinaryExprNode()) {
                    left = currentExpr
                    operator = operatorToken.lexeme
                    right = rightNode
                    this
                }
            }
        }
        return currentExpr
    }
}
 */