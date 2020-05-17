module UART.TxDemo where

import Clash.Prelude
import qualified UART.Tx as Tx
import qualified UART.ConfigGen as ConfigGen

-- Debounce a button input.
debouncer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
debouncer = mealy f 0
    where
        f :: Int -> Bool -> (Int, Bool)
        f 0 False = (0, False)
        f 0 True = (1, False)
        f x active | x == ConfigGen.buttonDebounce = case active of
            True -> (x, True)
            False -> (0, False)
        f x _ = (x + 1, False)

{-# ANN demo
    (Synthesize {
        t_name = "demo",
        t_inputs = [
            PortName "clk",
            PortName "rst",
            PortName "start",
            PortName "byte"
        ],
        t_output = PortName "output_bit"
    })
    #-}

demo :: Clock System -> Reset System
    -> Signal System Bool -- 发送按键
    -> Signal System (BitVector 8) -- 发送字节
    -> Signal System Bit -- 串口输出
demo clk rst = (exposeClockResetEnable demo') clk inversedRst (toEnable (pure True))
    where
        inversedRst = unsafeToReset $ fmap not $ unsafeFromReset rst
        demo' start byte = outputBit
            where
                (_, outputBit) = unbundle $ Tx.uartTx delay writeEnable byte -- 115200 baud rate
                delay = pure $ Tx.Delay ConfigGen.cyclesPerBit
                debouncedStart = debouncer (fmap not start) -- 按键按下: 0
                lastDebouncedStart = register False debouncedStart
                writeEnable = fmap calcWriteEnable $ bundle (debouncedStart, lastDebouncedStart)
                calcWriteEnable (True, False) = Tx.WriteEnable True
                calcWriteEnable _ = Tx.WriteEnable False
