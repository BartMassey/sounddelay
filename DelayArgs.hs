--- Copyright (C) 2007 Bart Massey
--- ALL RIGHTS RESERVED
--- Please see the end of this file for license information.

module DelayArgs
where

import System.Console.ParseArgs

data Options =
    OptionDelay |
    OptionAmplitude |
    OptionForward |
    OptionInputFile |
    OptionOutputFile
    deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [ Arg { argIndex = OptionDelay,
               argName = Just "delay",
               argAbbr = Just 'd',
               argData = argDataDefaulted "msecs" ArgtypeInt 0,
               argDesc = "Delay time" },
         Arg { argIndex = OptionAmplitude,
               argName = Just "amplitude",
               argAbbr = Just 'a',
               argData = argDataDefaulted "percent" ArgtypeDouble 100,
               argDesc = "Relative amplitude of delayed signal" },
         Arg { argIndex = OptionForward,
               argName = Just "forward",
               argAbbr = Just 'f',
               argData = Nothing,
               argDesc = "Signal is not circulated" },
         Arg { argIndex = OptionInputFile,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "input" ArgtypeString,
               argDesc = "Input file" },
         Arg { argIndex = OptionOutputFile,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "output" ArgtypeString,
               argDesc = "Output file" }]

--- Redistribution and use in source and binary forms, with or
--- without modification, are permitted provided that the
--- following conditions are met:
---     * Redistributions of source code must retain the above
---       copyright notice, this list of conditions and the following
---       disclaimer.
---     * Redistributions in binary form must reproduce the
---       above copyright notice, this list of conditions and the
---       following disclaimer in the documentation and/or other
---       materials provided with the distribution.
---     * Neither the name of Bart Massey, nor the names
---       of other affiliated organizations, nor the names
---       of other contributors may be used to endorse or promote
---       products derived from this software without specific prior
---       written permission.
--- 
--- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
--- CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
--- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
--- MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
--- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
--- NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
--- OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
