module UART.ConfigGen where

import Clash.Prelude
import UART.Config

cyclesPerBit = $(lift (clockRate `div` baudRate))
buttonDebounce = $(lift (clockRate `div` 100)) -- 10ms
