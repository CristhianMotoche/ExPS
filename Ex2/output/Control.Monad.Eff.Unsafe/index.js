// Generated by psc version 0.7.6.1
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var unsafePerformEff = function (_0) {
    return Control_Monad_Eff.runPure($foreign.unsafeInterleaveEff(_0));
};
module.exports = {
    unsafePerformEff: unsafePerformEff, 
    unsafeInterleaveEff: $foreign.unsafeInterleaveEff
};
