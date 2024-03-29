# lox-rust

An AST-walking interpreter of a lox-like language.
The primary objective with this project was to work on something interesting as I learned rust. 

Takeaways: 
  - Pattern matching is incredibly useful.
  - Fighting the borrow-checker is real.
  
Other notes:
- No inheritance support.
- No garbage collection.

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

///////////////////////////////////////////////////////////////
println "Cached lookups";
{
var a  = "*";

for(var i = 0; i < 9; i = i + 1) {    
    for(var j = 0; j < i; j = j + 1) {
        print a;
    }
    println "";
}

}
///////////////////////////////////////////////////////////////
println "Chaining function calls";
{
    fun A() {
        fun B() {
            fun C() {
                var message = "Hello World";
                println message;
            }
            return C;
        }
        return B;
    }
    A()()();
}
///////////////////////////////////////////////////////////////
println "Class support";
{
    class Thing {
        fun getCallback() {
        fun localFunction() {
            println this;
        }

        return localFunction;
        }
    }
    var callback = Thing().getCallback();
    callback();
}

{
  class Egotist {
    fun speak() {
      println this;
    }
  }

  var method = Egotist().speak;
  method();
}

{
  class Class {
    fun method() {
      println this;
    }
  }

  var instance1 = Class();
  instance1.prop = "instance1";
  var instance2 = Class();

  instance2.method = instance1.method;
  instance2.method(); // Calls instance1's method
}

{
  class Cake {
    fun taste() {
      var adjective = "delicious";
      println "The " + this.flavor + " cake is " + adjective + "!";
    }
  }

  var cake = Cake();
  cake.flavor = "German chocolate";
  cake.taste(); // Prints "The German chocolate cake is delicious!".
}
{
    class Class {
        fun init() {
            this.prop1 = "prop1 value";
            println "Hello world";
        }
    }

    var inst1 = Class();
    println inst1;
}
{
    class Class{
        fun init() {
            return;
        }
    }
    var instance1 = Class();    
    
    instance1.prop1 = 1;
    var instance2 = instance1;    
    instance2.prop2 = 2;

    println instance2.init(); // Calling "init: explicitly returns a reference to the original instance
}
{
    class Class {
        fun fibonacci(n) {
            if(n<=1)return n;
            return this.fibonacci(n-1) + this.fibonacci(n-2);
        }
    }
    for (var i = 0; i < 5; i = i + 1) {
        print i;
        print "'th fibonacci: ";
        println Class().fibonacci(i);
    }
}

///////////////////////////////////////////////////////////////
```
