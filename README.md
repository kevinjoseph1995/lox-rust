# lox-rust

WIP:


```
println "Scope and block statements";
{
    var a = "global a";
    var b = "global b";
    var c = "global c";
    {
    var a = "outer a";
    var b = "outer b";
    {
        var a = "inner a";
        println a;
        println b;
        println c;
    }
    println a;
    println b;
    println c;
    }
    println a;
    println b;
    println c;
}
{
    var a = 10;
    {
        println a;
        var b = a;
        println b;
    }
                    
    {
        {
            {
                {
                    var c = 99;
                    {
                        {
                            var b = a + 20;
                            println b;
                            b = c + a;
                            println b;
                        }
                    }
                }
            }
        }
    }
                        
    var b = 10;
    println b;
}
///////////////////////////////////////////////////////////////
println "While loops";
{
    var i = 10;    
    while(i >= 0) {        
        println i;
        i = i -1;        
    }
}
///////////////////////////////////////////////////////////////
println "For Loops";
{
    var i = 0;
    for(i = 0; i < 10; i = i + 1) {
        println i;
    }    
}
///////////////////////////////////////////////////////////////
println "Functions with recursion";
{
    fun fibonacci(n) {
    if (n <= 1) return n;
        return fibonacci(n - 2) + fibonacci(n - 1);
    }

    for (var i = 0; i < 5; i = i + 1) {
        print i;
        print "'th fibonacci: ";
        println fibonacci(i);
    }
}
///////////////////////////////////////////////////////////////
println "Capturing environment around declaration";
{
    fun outer_scoped() {
        var a = 10;
        fun scoped_function() {
            return a;
        }
        return scoped_function;
    }

    var inner_function = outer_scoped();
    // Prints 10
    println inner_function();
}
///////////////////////////////////////////////////////////////
println "Passing functions as value";
fun outer_function() {
    var message = "Hello World";
    fun inner_function() {
        fun inner_inner_function() {
            return message;            
        }
        return inner_inner_function();
    }
    return inner_function;
}
// message is not visible in this scope
var in = outer_function();
println in();

///////////////////////////////////////////////////////////////
println "Demonstrating variable resolution";
{
var a = "global";
{
  fun showA() {
    println a;
  }

  showA();
  var a = "block";
  showA();
}


fun function_a() {
    println "A";
}

{
    fun call_function_a() {
        function_a();
    }

    call_function_a(); // Should print A

    // Create a new function with the same name in current scope
    fun function_a() {
        println "B";
    }

    call_function_a(); // Should print "A"
    function_a(); // Should print "B"
}
}
```
