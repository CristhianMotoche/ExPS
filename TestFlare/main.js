// Generated by psc-bundle 0.8.2.0
var PS = { };
(function(exports) {
  /* global exports */
  "use strict";

  //- Bounded --------------------------------------------------------------------

  exports.topInt = 2147483647;
  exports.bottomInt = -2147483648;

  //- Show -----------------------------------------------------------------------

  exports.showIntImpl = function (n) {
    return n.toString();
  };
 
})(PS["Prelude"] = PS["Prelude"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var $foreign = PS["Prelude"];
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var Category = function (__superclass_Prelude$dotSemigroupoid_0, id) {
      this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
      this.id = id;
  };
  var Functor = function (map) {
      this.map = map;
  };
  var Apply = function (__superclass_Prelude$dotFunctor_0, apply) {
      this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
      this.apply = apply;
  };
  var Applicative = function (__superclass_Prelude$dotApply_0, pure) {
      this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
      this.pure = pure;
  };
  var Bind = function (__superclass_Prelude$dotApply_0, bind) {
      this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
      this.bind = bind;
  };
  var Monad = function (__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
      this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
      this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
  };
  var Bounded = function (bottom, top) {
      this.bottom = bottom;
      this.top = top;
  };
  var Show = function (show) {
      this.show = show;
  };                                                                           
  var unit = {};
  var top = function (dict) {
      return dict.top;
  };                                                 
  var showInt = new Show($foreign.showIntImpl);
  var show = function (dict) {
      return dict.show;
  };                                                                     
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });                 
  var pure = function (dict) {
      return dict.pure;
  };
  var $$return = function (dictApplicative) {
      return pure(dictApplicative);
  };
  var otherwise = true;
  var map = function (dict) {
      return dict.map;
  };
  var $less$dollar$greater = function (dictFunctor) {
      return map(dictFunctor);
  };
  var id = function (dict) {
      return dict.id;
  };
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  var compose = function (dict) {
      return dict.compose;
  };
  var categoryFn = new Category(function () {
      return semigroupoidFn;
  }, function (x) {
      return x;
  });
  var boundedInt = new Bounded($foreign.bottomInt, $foreign.topInt);
  var bottom = function (dict) {
      return dict.bottom;
  };
  var bind = function (dict) {
      return dict.bind;
  }; 
  var apply = function (dict) {
      return dict.apply;
  };
  var $less$times$greater = function (dictApply) {
      return apply(dictApply);
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return $less$times$greater(dictApplicative["__superclass_Prelude.Apply_0"]())(pure(dictApplicative)(f))(a);
          };
      };
  }; 
  var append = function (dict) {
      return dict.append;
  };
  var $less$greater = function (dictSemigroup) {
      return append(dictSemigroup);
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return bind(dictMonad["__superclass_Prelude.Bind_1"]())(f)(function (v) {
                  return bind(dictMonad["__superclass_Prelude.Bind_1"]())(a)(function (v1) {
                      return $$return(dictMonad["__superclass_Prelude.Applicative_0"]())(v(v1));
                  });
              });
          };
      };
  };
  exports["Show"] = Show;
  exports["Bounded"] = Bounded;
  exports["Monad"] = Monad;
  exports["Bind"] = Bind;
  exports["Applicative"] = Applicative;
  exports["Apply"] = Apply;
  exports["Functor"] = Functor;
  exports["Category"] = Category;
  exports["Semigroupoid"] = Semigroupoid;
  exports["show"] = show;
  exports["bottom"] = bottom;
  exports["top"] = top;
  exports["<>"] = $less$greater;
  exports["append"] = append;
  exports["ap"] = ap;
  exports["bind"] = bind;
  exports["liftA1"] = liftA1;
  exports["pure"] = pure;
  exports["<*>"] = $less$times$greater;
  exports["apply"] = apply;
  exports["<$>"] = $less$dollar$greater;
  exports["map"] = map;
  exports["id"] = id;
  exports["compose"] = compose;
  exports["otherwise"] = otherwise;
  exports["const"] = $$const;
  exports["unit"] = unit;
  exports["semigroupoidFn"] = semigroupoidFn;
  exports["categoryFn"] = categoryFn;
  exports["boundedInt"] = boundedInt;
  exports["showInt"] = showInt;;
 
})(PS["Prelude"] = PS["Prelude"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var Prelude = PS["Prelude"];
  var $times$greater = function (dictApply) {
      return function (a) {
          return function (b) {
              return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["const"](Prelude.id(Prelude.categoryFn)))(a))(b);
          };
      };
  };
  exports["*>"] = $times$greater;;
 
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Control.Monad.Eff

  exports.returnE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
 
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff"];
  var Prelude = PS["Prelude"];     
  var monadEff = new Prelude.Monad(function () {
      return applicativeEff;
  }, function () {
      return bindEff;
  });
  var bindEff = new Prelude.Bind(function () {
      return applyEff;
  }, $foreign.bindE);
  var applyEff = new Prelude.Apply(function () {
      return functorEff;
  }, Prelude.ap(monadEff));
  var applicativeEff = new Prelude.Applicative(function () {
      return applyEff;
  }, $foreign.returnE);
  var functorEff = new Prelude.Functor(Prelude.liftA1(applicativeEff));
  exports["functorEff"] = functorEff;
  exports["applyEff"] = applyEff;
  exports["applicativeEff"] = applicativeEff;
  exports["bindEff"] = bindEff;
  exports["monadEff"] = monadEff;;
 
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Foldable

  exports.foldrArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };

  exports.foldlArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };
 
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var Prelude = PS["Prelude"];
  var mempty = function (dict) {
      return dict.mempty;
  };
  exports["mempty"] = mempty;;
 
})(PS["Data.Monoid"] = PS["Data.Monoid"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var $foreign = PS["Data.Foldable"];
  var Prelude = PS["Prelude"];
  var Control_Apply = PS["Control.Apply"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Maybe_First = PS["Data.Maybe.First"];
  var Data_Maybe_Last = PS["Data.Maybe.Last"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Monoid_Additive = PS["Data.Monoid.Additive"];
  var Data_Monoid_Conj = PS["Data.Monoid.Conj"];
  var Data_Monoid_Disj = PS["Data.Monoid.Disj"];
  var Data_Monoid_Dual = PS["Data.Monoid.Dual"];
  var Data_Monoid_Endo = PS["Data.Monoid.Endo"];
  var Data_Monoid_Multiplicative = PS["Data.Monoid.Multiplicative"];     
  var Foldable = function (foldMap, foldl, foldr) {
      this.foldMap = foldMap;
      this.foldl = foldl;
      this.foldr = foldr;
  };
  var foldr = function (dict) {
      return dict.foldr;
  };
  var traverse_ = function (dictApplicative) {
      return function (dictFoldable) {
          return function (f) {
              return foldr(dictFoldable)(function ($161) {
                  return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(f($161));
              })(Prelude.pure(dictApplicative)(Prelude.unit));
          };
      };
  };
  var foldl = function (dict) {
      return dict.foldl;
  }; 
  var foldMapDefaultR = function (dictFoldable) {
      return function (dictMonoid) {
          return function (f) {
              return function (xs) {
                  return foldr(dictFoldable)(function (x) {
                      return function (acc) {
                          return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                      };
                  })(Data_Monoid.mempty(dictMonoid))(xs);
              };
          };
      };
  };
  var foldableArray = new Foldable(function (dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
  }, $foreign.foldlArray, $foreign.foldrArray);
  var foldMap = function (dict) {
      return dict.foldMap;
  };
  exports["Foldable"] = Foldable;
  exports["traverse_"] = traverse_;
  exports["foldMapDefaultR"] = foldMapDefaultR;
  exports["foldMap"] = foldMap;
  exports["foldl"] = foldl;
  exports["foldr"] = foldr;
  exports["foldableArray"] = foldableArray;;
 
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});
(function(exports) {
  // module Flare
  // jshint browser: true
  // jshint node: true

  "use strict";

  exports.renderString = function(target) {
    return function(content) {
      return function() {
        document.getElementById(target).innerHTML = content;
      };
    };
  };

  exports.removeChildren = function(target) {
    return function() {
      var el = document.getElementById(target);

      // http://stackoverflow.com/a/3955238/704831
      while (el.firstChild) {
        el.removeChild(el.firstChild);
      }
    };
  };

  exports.appendComponent = function(target) {
    return function(el) {
      return function() {
        document.getElementById(target).appendChild(el);
      };
    };
  };

  // This function maintains a global state `window.flareID` to generate unique
  // DOM element IDs. It is only called from functions with a DOM effect.
  function getUniqueID() {
    if (window.flareID === undefined) {
      window.flareID = 0;
    }
    window.flareID = window.flareID + 1;
    return "flare-component-" + window.flareID.toString();
  }

  function createComponent(inputType, elementCallback, eventType, eventListener) {
    return function(label) {
      return function(initial) {
        return function(send) {
          return function() {
            var uid = getUniqueID();
            var el = elementCallback(initial);
            el.className = "flare-input-" + inputType;
            el.id = uid;

            var div = document.createElement("div");
            div.className = "flare-input";

            if (label !== "") {
              var labelEl = document.createElement("label");
              labelEl.htmlFor = uid;
              labelEl.appendChild(document.createTextNode(label));
              div.appendChild(labelEl);
            }

            div.appendChild(el);

            el.addEventListener(eventType, function(e) {
              var value = eventListener(e.target, initial);
              send(value)();
            });

            return div;
          };
        };
      };
    };
  } 

  function clamp(min, max, initial, value) {
    if (isNaN(value)) {
      return initial;
    } else if (value < min) {
      return min;
    } else if (value > max) {
      return max;
    }
    return value;
  } 

  exports.cIntRange = function(type) {
    return function(min) {
      return function(max) {
        return createComponent("int-" + type,
          function(initial) {
            var input = document.createElement("input");
            input.type = type;
            input.min = min.toString();
            input.max = max.toString();
            input.step = "1";
            input.value = initial.toString();
            return input;
          },
          "input",
          function(t, initial) {
            return clamp(min, max, initial, parseInt(t.value, 10));
          }
        );
      };
    };
  };

  // vim: ts=2:sw=2
 
})(PS["Flare"] = PS["Flare"] || {});
(function(exports) {
  // module Signal

  exports.constant =
    function constant(initial) {
      var subs = [];
      var val = initial;
      var sig = {
        subscribe: function(sub) {
          subs.push(sub);
          sub(val);
        },
        get: function() { return val; },
        set: function(newval) {
          val = newval;
          subs.forEach(function(sub) { sub(newval); });
        }
      };
      return sig;
    };

  exports.mapSigP =
    function mapSigP(constant) {
      return function(fun) {
        return function(sig) {
          var out = constant(fun(sig.get()));
          sig.subscribe(function(val) { out.set(fun(val)); });
          return out;
        };
      };
    };

  exports.runSignal =
    function runSignal(sig) {
      return function() {
        sig.subscribe(function(val) {
          val();
        });
        return {};
      };
    };
 
})(PS["Signal"] = PS["Signal"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var $foreign = PS["Signal"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Prelude = PS["Prelude"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Maybe = PS["Data.Maybe"];                 
  var mapSig = $foreign.mapSigP($foreign.constant);
  var functorSignal = new Prelude.Functor(mapSig);
  exports["functorSignal"] = functorSignal;
  exports["runSignal"] = $foreign.runSignal;
  exports["constant"] = $foreign.constant;;
 
})(PS["Signal"] = PS["Signal"] || {});
(function(exports) {
  // module Signal.Channel

  exports.channelP =
    function channelP(constant) {
      return function(v) {
        return function() {
          return constant(v);
        };
      };
    };

  exports.sendP =
    function sendP(chan, v) {
      return function(v) {
        return function() {
          chan.set(v);
        };
      };
    };

  exports.subscribe =
    function subscribe(chan) {
      return chan;
    };
 
})(PS["Signal.Channel"] = PS["Signal.Channel"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var $foreign = PS["Signal.Channel"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Prelude = PS["Prelude"];
  var Signal = PS["Signal"];     
  var send = $foreign.sendP;
  var channel = $foreign.channelP(Signal.constant);
  exports["send"] = send;
  exports["channel"] = channel;
  exports["subscribe"] = $foreign.subscribe;;
 
})(PS["Signal.Channel"] = PS["Signal.Channel"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var $foreign = PS["Flare"];
  var Prelude = PS["Prelude"];
  var Data_Array = PS["Data.Array"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Traversable = PS["Data.Traversable"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Color = PS["Color"];
  var DOM = PS["DOM"];
  var DOM_Node_Types = PS["DOM.Node.Types"];
  var Signal = PS["Signal"];
  var Signal_Channel = PS["Signal.Channel"];     
  var Flare = (function () {
      function Flare(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Flare.create = function (value0) {
          return function (value1) {
              return new Flare(value0, value1);
          };
      };
      return Flare;
  })();
  var UI = function (x) {
      return x;
  };
  var runFlareWith = function (controls) {
      return function (handler) {
          return function (v) {
              return function __do() {
                  var v1 = v();
                  $foreign.removeChildren(controls)();
                  Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)($foreign.appendComponent(controls))(v1.value0)();
                  return Signal.runSignal(Prelude.map(Signal.functorSignal)(handler)(v1.value1))();
              };
          };
      };
  };
  var runFlare = function (controls) {
      return function (target) {
          return runFlareWith(controls)($foreign.renderString(target));
      };
  };
  var functorFlare = new Prelude.Functor(function (f) {
      return function (v) {
          return new Flare(v.value0, Prelude.map(Signal.functorSignal)(f)(v.value1));
      };
  });
  var functorUI = new Prelude.Functor(function (f) {
      return function (v) {
          return UI(Prelude.map(Control_Monad_Eff.functorEff)(Prelude.map(functorFlare)(f))(v));
      };
  });
  var createUI = function (createComp) {
      return function (label) {
          return function ($$default) {
              return UI(function __do() {
                  var v = Signal_Channel.channel($$default)();
                  var v1 = createComp(label)($$default)(Signal_Channel.send(v))();
                  var signal = Signal_Channel.subscribe(v);
                  return new Flare([ v1 ], signal);
              });
          };
      };
  };
  var $$int = function (label) {
      return createUI($foreign.cIntRange("number")(Prelude.bottom(Prelude.boundedInt))(Prelude.top(Prelude.boundedInt)))(label);
  };
  exports["runFlare"] = runFlare;
  exports["runFlareWith"] = runFlareWith;
  exports["int"] = $$int;
  exports["functorFlare"] = functorFlare;
  exports["functorUI"] = functorUI;;
 
})(PS["Flare"] = PS["Flare"] || {});
(function(exports) {
  // Generated by psc version 0.8.2.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Flare = PS["Flare"];     
  var fib = function (n) {
      if (n <= 0) {
          return 1;
      };
      if (n === 1) {
          return 1;
      };
      if (Prelude.otherwise) {
          return fib(n - 1) + fib(n - 2) | 0;
      };
      throw new Error("Failed pattern match at Main line 8, column 1 - line 13, column 1: " + [ n.constructor.name ]);
  };
  var fib$prime = Prelude["<$>"](Flare.functorUI)(fib)(Flare["int"]("Fibonnaci")(3));
  var ui = Prelude["<$>"](Flare.functorUI)(Prelude.show(Prelude.showInt))(fib$prime);
  var main = Flare.runFlare("Multiplication")("Output")(ui);
  exports["main"] = main;
  exports["ui"] = ui;
  exports["fib'"] = fib$prime;
  exports["fib"] = fib;;
 
})(PS["Main"] = PS["Main"] || {});

PS["Main"].main();
