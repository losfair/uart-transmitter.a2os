module UART.Tx where

import Clash.Prelude

newtype Delay = Delay (Unsigned 32)
    deriving (Generic, NFDataX)

newtype WriteEnable = WriteEnable Bool
    deriving (Generic, NFDataX)

newtype WriteActive = WriteActive Bool
    deriving (Generic, NFDataX)

data TxState = Idle | Sending Delay DelayedTxState
    deriving (Generic, NFDataX)

data DelayedTxState = WillSend (Index 8) (BitVector 8) Delay
    | WillSendLast Delay
    | WillStop (Index 2) Delay
    deriving (Generic, NFDataX)

uartTx :: HiddenClockResetEnable dom
    => Signal dom Delay -- 每个位保持的时间（时钟周期数）
    -> Signal dom WriteEnable -- 写 enable
    -> Signal dom (BitVector 8) -- 要发送的字节
    -> Signal dom (WriteActive, Bit) -- (正在发送, 位输出)
uartTx delay we byte = mealy cycle (Idle, high) $ bundle (delay, we, byte)
    where
        cycle (state, bit) input = (updateState state input bit, (writeActive, bit))
            where
                writeActive = case (state, input) of
                    (Idle, (_, WriteEnable False, _)) -> WriteActive False
                    _ -> WriteActive True

updateState :: TxState -> (Delay, WriteEnable, BitVector 8) -> Bit -> (TxState, Bit)
updateState state input lastBit = case (state, input) of
    (Idle, (_, WriteEnable False, _)) -> (Idle, high)
    (Idle, (delay, WriteEnable True, byte)) -> (Sending delay $ WillSend 0 byte delay, low)
    (Sending (Delay 0) inner, _) -> updateSendingState inner
    (Sending (Delay d) inner, _) -> (Sending (Delay $ d - 1) inner, lastBit)

updateSendingState (WillSend i byte totalDelay) = (nextState, boolToBit $ testBit byte 0)
    where
        nextState = Sending totalDelay $
            if i == maxBound then
                WillSendLast totalDelay
            else
                WillSend (i + 1) (shiftR byte 1) totalDelay

updateSendingState (WillSendLast totalDelay) = (Sending totalDelay $ WillStop 0 totalDelay, high)

updateSendingState (WillStop 0 totalDelay) = (Sending totalDelay $ WillStop 1 totalDelay, high)
updateSendingState (WillStop 1 totalDelay) = (Idle, high)
