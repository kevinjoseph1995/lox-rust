# lox-rust

WIP:


```
print "Scope and block statements";
{
    var a = "global a";
    var b = "global b";
    var c = "global c";
    {
    var a = "outer a";
    var b = "outer b";
    {
        var a = "inner a";
        print a;
        print b;
        print c;
    }
    print a;
    print b;
    print c;
    }
    print a;
    print b;
    print c;
}
{
    var a = 10;
    {
        print a;
        var b = a;
        print b;
    }
                    
    {
        {
            {
                {
                    var c = 99;
                    {
                        {
                            var b = a + 20;
                            print b;
                            b = c + a;
                            print b;
                        }
                    }
                }
            }
        }
    }
                        
    var b = 10;
    print b;
}

print "While loops";
{
    var i = 10;    
    while(i >= 0) {        
        print i;
        i = i -1;        
    }
}

print "For Loops";
{
    var i = 0;
    for(i = 0; i < 10; i = i + 1) {
        print i;
    }
    
}```
