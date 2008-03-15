--- Copyright (C) 2007 Bart Massey
--- ALL RIGHTS RESERVED
--- Please see the end of this file for license information.

module Main
where

import Data.List
import Data.Maybe
import System.Environment
import System.Console.ParseArgs
import Data.WAVE
import DelayArgs

main :: IO ()
main = do
    args <- parseArgsIO ArgsComplete argd
    --- get the input
    h <- getArgStdio args OptionInputFile ReadMode
    wav <- hGetWAVE h
    let samples =  map (map sampleToDouble) (waveSamples wav)
    --- figure the parameters
    let hd = waveHeader wav
    let rate = waveFrameRate hd
    let delay = fromJust (getArgInt args OptionDelay)
    let delay_samples = rate * delay `div` 1000
    let amplitude = fromJust (getArgDouble args OptionAmplitude) / 100
    let silent_frame = replicate (waveNumChannels hd) 0.0
    --- run delay program on the input
    let samples' = map (map (* (1.0 - amplitude))) samples
    let mix = if gotArg args OptionForward
              then repeat silent_frame
              else samples'
    let mix' = map (map (* amplitude)) mix
    let samples'' = replicate delay_samples silent_frame ++ samples'
    let result = zipWith (zipWith (+)) mix' samples''
    --- write the output
    let samples' = map (map doubleToSample) result
    let wav' = WAVE { waveHeader = hd,
                      waveSamples = samples' }
    h' <- getArgStdio args OptionOutputFile WriteMode
    hPutWAVE h' wav'


    
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
