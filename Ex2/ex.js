var PS={};(function(a){a.log=function(a){return function(){console.log(a);return{}}}})(PS["Control.Monad.Eff.Console"]=PS["Control.Monad.Eff.Console"]||{});
(function(a){a.arrayMap=function(a){return function(c){for(var d=c.length,e=Array(d),f=0;f<d;f++)e[f]=a(c[f]);return e}};a.showIntImpl=function(a){return a.toString()};a.showNumberImpl=function(a){return a===(a|0)?a+".0":a.toString()};a.showArrayImpl=function(a){return function(c){for(var d=[],e=0,f=c.length;e<f;e++)d[e]=a(c[e]);return"["+d.join(",")+"]"}}})(PS.Prelude=PS.Prelude||{});
(function(a){var b=PS.Prelude,c=function(a){this.map=a},d=function(a){this.show=a},e=new d(b.showNumberImpl),f=new d(b.showIntImpl),g=new c(b.arrayMap);a.Show=d;a.Functor=c;a.show=function(a){return a.show};a.map=function(a){return a.map};a.functorArray=g;a.showInt=f;a.showNumber=e;a.showArray=function(a){return new d(b.showArrayImpl(a.show))}})(PS.Prelude=PS.Prelude||{});
(function(a){var b=PS["Control.Monad.Eff.Console"],c=PS.Prelude;a.print=function(a){return function(e){return b.log(c.show(a)(e))}};a.log=b.log})(PS["Control.Monad.Eff.Console"]=PS["Control.Monad.Eff.Console"]||{});(function(a){a.unsafeStringify=function(a){return JSON.stringify(a)}})(PS["Global.Unsafe"]=PS["Global.Unsafe"]||{});(function(a){a.unsafeStringify=PS["Global.Unsafe"].unsafeStringify})(PS["Global.Unsafe"]=PS["Global.Unsafe"]||{});
(function(a){a.sqrt=Math.sqrt;a.pi=Math.PI})(PS.Math=PS.Math||{});(function(a){var b=PS.Math;a.pi=b.pi;a.sqrt=b.sqrt})(PS.Math=PS.Math||{});
(function(a){var b=PS.Prelude,c=PS["Control.Monad.Eff.Console"],d=PS.Math,e=PS["Global.Unsafe"],f=function(a){return function(b){return d.sqrt(a*a+b*b)}},g=function(a){return 2*d.pi*d.sqrt(a)};a.circleArea=g;a.diagonal=f;a.main=function(){c.log("Hello CAMM!")();c.log(b.show(b.showArray(b.showInt))(b.map(b.functorArray)(function(a){return a+3|0})([1,2,3])))();c.print(b.showNumber)(f(10)(10))();return c.log(e.unsafeStringify(g(10)))()}})(PS.Main=PS.Main||{});PS.Main.main();
