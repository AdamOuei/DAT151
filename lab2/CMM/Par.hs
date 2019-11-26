{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module CMM.Par where
import CMM.Abs
import CMM.Lex
import CMM.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn23 :: (Integer) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Integer)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Double) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Double)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Id) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Id)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Program) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Program)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Func) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Func)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Argument) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Argument)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Args) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Args)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Body) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Body)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ([Args]) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ([Args])
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([Func]) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([Func])
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Type) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Type)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([Id]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([Id])
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Stm) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Stm)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ([Stm]) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ([Stm])
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Exp) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Exp)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Exp) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Exp)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Exp) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Exp)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Exp) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Exp)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Exp) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Exp)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Exp) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Exp)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Exp) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Exp)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Exp) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Exp)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([Exp]) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([Exp])
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x80\x48\x80\xfd\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x00\x20\x24\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x80\xfd\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x80\xfd\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x80\x48\x80\xfd\x7b\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x00\x44\x70\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x80\xfd\x73\x00\x00\x00\x00\x00\x80\x48\x80\xfd\x73\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x48\x80\xfd\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pFunc","%start_pArgument","%start_pArgs","%start_pBody","%start_pListArgs","%start_pListFunc","%start_pType","%start_pListId","%start_pStm","%start_pListStm","%start_pExp7","%start_pExp6","%start_pExp5","%start_pExp4","%start_pExp3","%start_pExp2","%start_pExp1","%start_pExp","%start_pListExp","Integer","Double","Id","Program","Func","Argument","Args","Body","ListArgs","ListFunc","Type","ListId","Stm","ListStm","Exp7","Exp6","Exp5","Exp4","Exp3","Exp2","Exp1","Exp","ListExp","'!='","'&&'","'('","')'","'*'","'+'","'++'","','","'-'","'--'","'/'","';'","'<'","'<='","'='","'=='","'>'","'>='","'bool'","'double'","'else'","'false'","'if'","'int'","'return'","'true'","'void'","'while'","'{'","'||'","'}'","L_integ","L_doubl","L_Id","%eof"]
        bit_start = st * 80
        bit_end = (st + 1) * 80
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..79]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x39\x00\x01\x00\x39\x00\xe8\xff\x39\x00\x00\x00\x39\x00\xf0\xff\x64\x00\x00\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\xf4\xff\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x0a\x00\xca\x01\x00\x00\x2b\x00\x11\x00\x00\x00\x29\x00\x16\x00\x80\x00\x2e\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x30\x00\x69\x00\xe4\xff\xff\xff\x30\x00\x05\x00\xfe\xff\x30\x00\x03\x00\x40\x00\x4e\x00\x61\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x80\x00\x00\x00\x71\x00\x00\x00\x9b\x00\x56\x00\x56\x00\x0a\x02\x9c\x00\x8f\x00\x92\x00\x93\x00\x00\x00\x93\x00\x93\x00\x39\x00\x93\x00\x98\x00\xaf\x00\x39\x00\x00\x00\x88\x00\xcd\x00\x24\x00\x00\x00\x39\x00\x99\x00\x44\x00\x80\x00\xc7\x00\x80\x00\x00\x00\x4a\x00\xc8\x00\x00\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x80\x00\x00\x00\x83\x00\x83\x00\x83\x00\x83\x00\x83\x00\x83\x00\x00\x00\x00\x00\xd2\x00\xd5\x00\x00\x00\x0a\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x80\x00\xd7\x00\x00\x00\xe3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x00\x00\x00\x64\x00\x64\x00\xdc\x00\x00\x00\x00\x00\xd4\x00\x00\x00\x64\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x4c\x00\x6b\x00\xe5\x00\x26\x00\xe6\x00\x8a\x00\xe2\x00\xe7\x00\x06\x00\xce\x00\xdf\x00\x34\x00\x15\x02\xe6\x01\xd4\x01\x8d\x00\xb2\x01\x96\x01\x3c\x01\x97\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x01\xed\x00\xfb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x01\x00\x00\x00\x00\xf1\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\xfd\x00\x00\x00\xf3\x00\x00\x00\x00\x00\x8b\x00\x00\x00\xff\x00\x00\x00\x7e\x00\x00\x00\xfe\x00\x00\x00\xe4\x00\x00\x00\xab\x00\x28\x00\xe4\x00\x62\x01\x00\x00\x78\x01\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x68\x00\x26\x02\x29\x02\xdd\x01\xb8\x01\xae\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x00\xef\x01\xf7\x01\xfa\x01\x00\x02\x0b\x02\x11\x02\x9d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\xfa\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x01\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\xe0\xff\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\xff\x00\x00\xeb\xff\xcf\xff\xce\xff\xcb\xff\xc2\xff\xbf\xff\xb8\xff\xb6\xff\xb4\xff\xb2\xff\xb1\xff\xaf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xcd\xff\xea\xff\xe9\xff\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xde\xff\x00\x00\xdc\xff\x00\x00\xdb\xff\x00\x00\xd1\xff\xda\xff\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\xe8\xff\xdf\xff\x00\x00\x00\x00\x00\x00\xe5\xff\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\xda\xff\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\xff\xc9\xff\xc8\xff\xc6\xff\xc7\xff\x00\x00\xb0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\xbb\xff\xbd\xff\xba\xff\xbc\xff\xbe\xff\xb9\xff\xae\xff\xc5\xff\x00\x00\xb5\xff\xb7\xff\xc0\xff\xc1\xff\xc3\xff\xc4\xff\xd7\xff\x00\x00\x00\x00\xd5\xff\x00\x00\xd2\xff\xd9\xff\xe1\xff\xe4\xff\xe6\xff\x00\x00\xe7\xff\x00\x00\x00\x00\x00\x00\xca\xff\xd6\xff\x00\x00\xd4\xff\x00\x00\xd3\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x1e\x00\x05\x00\x03\x00\x1d\x00\x03\x00\x23\x00\x02\x00\x0b\x00\x07\x00\x06\x00\x03\x00\x0a\x00\x09\x00\x05\x00\x07\x00\x0b\x00\x22\x00\x0a\x00\x20\x00\x0b\x00\x13\x00\x14\x00\x0f\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x23\x00\x23\x00\x20\x00\x21\x00\x22\x00\x23\x00\x03\x00\x23\x00\x02\x00\x02\x00\x07\x00\x06\x00\x02\x00\x0a\x00\x1e\x00\x0a\x00\x08\x00\x0b\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x13\x00\x14\x00\x23\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x03\x00\x00\x00\x01\x00\x02\x00\x07\x00\x13\x00\x14\x00\x0a\x00\x03\x00\x22\x00\x18\x00\x08\x00\x23\x00\x1b\x00\x09\x00\x0e\x00\x13\x00\x14\x00\x0f\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x22\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x03\x00\x00\x00\x01\x00\x02\x00\x07\x00\x03\x00\x0c\x00\x0a\x00\x04\x00\x07\x00\x23\x00\x03\x00\x0a\x00\x03\x00\x0a\x00\x0e\x00\x13\x00\x14\x00\x23\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x04\x00\x03\x00\x20\x00\x21\x00\x22\x00\x07\x00\x0a\x00\x06\x00\x0a\x00\x03\x00\x09\x00\x00\x00\x01\x00\x02\x00\x06\x00\x06\x00\x08\x00\x08\x00\x0a\x00\x0a\x00\x16\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x20\x00\x21\x00\x22\x00\x08\x00\x08\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x06\x00\x23\x00\x08\x00\x22\x00\x0a\x00\x23\x00\x00\x00\x01\x00\x02\x00\x22\x00\x22\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x04\x00\x23\x00\x0c\x00\x0c\x00\x04\x00\x04\x00\x02\x00\x0a\x00\x1d\x00\x0c\x00\x04\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x04\x00\x0c\x00\x15\x00\x05\x00\x09\x00\x0d\x00\x07\x00\x0a\x00\x02\x00\x0c\x00\x0a\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x02\x00\x0d\x00\x02\x00\x0d\x00\x02\x00\xff\xff\x05\x00\x0a\x00\xff\xff\x0c\x00\x07\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x0c\x00\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\x06\x00\xff\xff\xff\xff\x09\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\x13\x00\x14\x00\x0e\x00\x0f\x00\x10\x00\x18\x00\x0e\x00\x0f\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\xff\xff\x23\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\xff\xff\x0e\x00\x0f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x62\x00\x63\x00\x5e\x00\x4a\x00\x47\x00\x23\x00\xff\xff\x3e\x00\x5f\x00\x24\x00\x60\x00\x64\x00\x25\x00\x61\x00\x5e\x00\x65\x00\x3f\x00\x29\x00\x66\x00\x16\x00\x5f\x00\x37\x00\x38\x00\x71\x00\x26\x00\x39\x00\x3a\x00\x3b\x00\x27\x00\x3c\x00\x3d\x00\x3e\x00\xff\xff\xff\xff\x16\x00\x28\x00\x29\x00\xff\xff\x23\x00\xff\xff\x5a\x00\x3e\x00\x24\x00\x47\x00\x62\x00\x25\x00\x63\x00\x44\x00\x6a\x00\x5b\x00\x87\x00\x16\x00\x17\x00\x2b\x00\x37\x00\x38\x00\xff\xff\x26\x00\x39\x00\x3a\x00\x3b\x00\x27\x00\x3c\x00\x3d\x00\x3e\x00\x31\x00\x8a\x00\x16\x00\x28\x00\x29\x00\x23\x00\x16\x00\x17\x00\x2b\x00\x24\x00\x37\x00\x38\x00\x25\x00\x4c\x00\x29\x00\x3a\x00\x55\x00\xff\xff\x3c\x00\x4d\x00\x80\x00\x37\x00\x38\x00\x83\x00\x26\x00\x39\x00\x3a\x00\x3b\x00\x27\x00\x3c\x00\x3d\x00\x3e\x00\x29\x00\x87\x00\x16\x00\x28\x00\x29\x00\x23\x00\x16\x00\x17\x00\x2b\x00\x24\x00\x64\x00\x5a\x00\x25\x00\x4a\x00\x65\x00\xff\xff\x59\x00\x66\x00\x57\x00\x4b\x00\x7f\x00\x37\x00\x38\x00\xff\xff\x26\x00\x39\x00\x3a\x00\x3b\x00\x27\x00\x3c\x00\x3d\x00\x3e\x00\x4e\x00\x23\x00\x16\x00\x28\x00\x29\x00\x24\x00\x4b\x00\x60\x00\x25\x00\x4a\x00\x61\x00\x16\x00\x17\x00\x2b\x00\x42\x00\x42\x00\x43\x00\x50\x00\x44\x00\x44\x00\x26\x00\x16\x00\x17\x00\x18\x00\x27\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x2d\x00\x16\x00\x28\x00\x29\x00\x55\x00\x54\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x16\x00\x17\x00\x18\x00\x42\x00\xff\xff\x88\x00\x29\x00\x44\x00\xff\xff\x16\x00\x17\x00\x18\x00\x29\x00\x29\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x7a\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x78\x00\x16\x00\x17\x00\x18\x00\x8b\x00\xff\xff\x85\x00\x82\x00\x7a\x00\x91\x00\x62\x00\x33\x00\x47\x00\x34\x00\x8f\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x35\x00\x16\x00\x17\x00\x18\x00\x8e\x00\x92\x00\x95\x00\x48\x00\x41\x00\x32\x00\x45\x00\x33\x00\x67\x00\x5c\x00\x40\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x35\x00\x16\x00\x17\x00\x18\x00\x66\x00\x55\x00\x52\x00\x51\x00\x4f\x00\x00\x00\x8b\x00\x33\x00\x00\x00\x93\x00\x8c\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x35\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x92\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x35\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x95\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x35\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x29\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x68\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x57\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x85\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x83\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x8f\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x2a\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x71\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x2c\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x7b\x00\x6b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x61\x00\x16\x00\x17\x00\x2b\x00\x6c\x00\x6d\x00\x00\x00\x6e\x00\x6f\x00\x70\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x2e\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x1b\x00\x7c\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x2f\x00\x16\x00\x17\x00\x2b\x00\x16\x00\x17\x00\x2b\x00\x19\x00\x1a\x00\x77\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x00\x00\x19\x00\x1a\x00\x76\x00\x19\x00\x1a\x00\x75\x00\x16\x00\x17\x00\x2b\x00\x19\x00\x1a\x00\x74\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x16\x00\x17\x00\x2b\x00\x00\x00\x19\x00\x1a\x00\x73\x00\x00\x00\x37\x00\x38\x00\x19\x00\x1a\x00\x72\x00\x3a\x00\x19\x00\x30\x00\x3c\x00\x16\x00\x17\x00\x2b\x00\x16\x00\x17\x00\x2b\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x7e\x00\x00\x00\x19\x00\x7d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (20, 81) [
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81)
	]

happy_n_terms = 36 :: Int
happy_n_nonterms = 23 :: Int

happyReduce_20 = happySpecReduce_1  0# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn23
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_21 = happySpecReduce_1  1# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn24
		 ((read ( happy_var_1)) :: Double
	)}

happyReduce_22 = happySpecReduce_1  2# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Id happy_var_1)) -> 
	happyIn25
		 (Id (happy_var_1)
	)}

happyReduce_23 = happySpecReduce_1  3# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (CMM.Abs.Prg (reverse happy_var_1)
	)}

happyReduce_24 = happyReduce 4# 4# happyReduction_24
happyReduction_24 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn27
		 (CMM.Abs.FDef happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_25 = happySpecReduce_3  5# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (CMM.Abs.FArgument happy_var_2
	)}

happyReduce_26 = happySpecReduce_2  6# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (CMM.Abs.FArgs happy_var_1 happy_var_2
	)}}

happyReduce_27 = happySpecReduce_3  7# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (CMM.Abs.FBody (reverse happy_var_2)
	)}

happyReduce_28 = happySpecReduce_0  8# happyReduction_28
happyReduction_28  =  happyIn31
		 ([]
	)

happyReduce_29 = happySpecReduce_1  8# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((:[]) happy_var_1
	)}

happyReduce_30 = happySpecReduce_3  8# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_0  9# happyReduction_31
happyReduction_31  =  happyIn32
		 ([]
	)

happyReduce_32 = happySpecReduce_2  9# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_33 = happySpecReduce_1  10# happyReduction_33
happyReduction_33 happy_x_1
	 =  happyIn33
		 (CMM.Abs.TDouble
	)

happyReduce_34 = happySpecReduce_1  10# happyReduction_34
happyReduction_34 happy_x_1
	 =  happyIn33
		 (CMM.Abs.TBool
	)

happyReduce_35 = happySpecReduce_1  10# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn33
		 (CMM.Abs.TInt
	)

happyReduce_36 = happySpecReduce_1  10# happyReduction_36
happyReduction_36 happy_x_1
	 =  happyIn33
		 (CMM.Abs.TVoid
	)

happyReduce_37 = happySpecReduce_1  11# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((:[]) happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  11# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_2  12# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (CMM.Abs.SExp happy_var_1
	)}

happyReduce_40 = happySpecReduce_3  12# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (CMM.Abs.SDecls happy_var_1 happy_var_2
	)}}

happyReduce_41 = happyReduce 5# 12# happyReduction_41
happyReduction_41 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_4 of { happy_var_4 -> 
	happyIn35
		 (CMM.Abs.SInit happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_42 = happySpecReduce_3  12# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (CMM.Abs.SRet happy_var_2
	)}

happyReduce_43 = happyReduce 5# 12# happyReduction_43
happyReduction_43 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut44 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_5 of { happy_var_5 -> 
	happyIn35
		 (CMM.Abs.SWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_44 = happyReduce 7# 12# happyReduction_44
happyReduction_44 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut44 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_5 of { happy_var_5 -> 
	case happyOut35 happy_x_7 of { happy_var_7 -> 
	happyIn35
		 (CMM.Abs.SIf happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_45 = happySpecReduce_3  12# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (CMM.Abs.SBlock (reverse happy_var_2)
	)}

happyReduce_46 = happySpecReduce_0  13# happyReduction_46
happyReduction_46  =  happyIn36
		 ([]
	)

happyReduce_47 = happySpecReduce_2  13# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_48 = happySpecReduce_1  14# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (CMM.Abs.EInt happy_var_1
	)}

happyReduce_49 = happySpecReduce_1  14# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (CMM.Abs.EDouble happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  14# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn37
		 (CMM.Abs.ETrue
	)

happyReduce_51 = happySpecReduce_1  14# happyReduction_51
happyReduction_51 happy_x_1
	 =  happyIn37
		 (CMM.Abs.EFalse
	)

happyReduce_52 = happySpecReduce_1  14# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (CMM.Abs.EId happy_var_1
	)}

happyReduce_53 = happyReduce 4# 14# happyReduction_53
happyReduction_53 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (CMM.Abs.ECall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_54 = happySpecReduce_2  14# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (CMM.Abs.EInc happy_var_1
	)}

happyReduce_55 = happySpecReduce_2  14# happyReduction_55
happyReduction_55 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (CMM.Abs.EDec happy_var_1
	)}

happyReduce_56 = happySpecReduce_2  14# happyReduction_56
happyReduction_56 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (CMM.Abs.EInc2 happy_var_2
	)}

happyReduce_57 = happySpecReduce_2  14# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (CMM.Abs.EDec2 happy_var_2
	)}

happyReduce_58 = happySpecReduce_3  14# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (happy_var_2
	)}

happyReduce_59 = happySpecReduce_3  15# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (CMM.Abs.EMul happy_var_1 happy_var_3
	)}}

happyReduce_60 = happySpecReduce_3  15# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (CMM.Abs.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_61 = happySpecReduce_1  15# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_62 = happySpecReduce_3  16# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (CMM.Abs.EAdd happy_var_1 happy_var_3
	)}}

happyReduce_63 = happySpecReduce_3  16# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (CMM.Abs.ESub happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_1  16# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  17# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (CMM.Abs.ELess happy_var_1 happy_var_3
	)}}

happyReduce_66 = happySpecReduce_3  17# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (CMM.Abs.EGre happy_var_1 happy_var_3
	)}}

happyReduce_67 = happySpecReduce_3  17# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (CMM.Abs.ELeq happy_var_1 happy_var_3
	)}}

happyReduce_68 = happySpecReduce_3  17# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (CMM.Abs.EGeq happy_var_1 happy_var_3
	)}}

happyReduce_69 = happySpecReduce_3  17# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (CMM.Abs.EEqua happy_var_1 happy_var_3
	)}}

happyReduce_70 = happySpecReduce_3  17# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (CMM.Abs.EIneq happy_var_1 happy_var_3
	)}}

happyReduce_71 = happySpecReduce_1  17# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_72 = happySpecReduce_3  18# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (CMM.Abs.EConj happy_var_1 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_1  18# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_74 = happySpecReduce_3  19# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (CMM.Abs.EDisj happy_var_1 happy_var_3
	)}}

happyReduce_75 = happySpecReduce_1  19# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_76 = happySpecReduce_3  20# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (CMM.Abs.EAss happy_var_1 happy_var_3
	)}}

happyReduce_77 = happySpecReduce_1  20# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_78 = happySpecReduce_1  21# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_79 = happySpecReduce_0  22# happyReduction_79
happyReduction_79  =  happyIn45
		 ([]
	)

happyReduce_80 = happySpecReduce_1  22# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ((:[]) happy_var_1
	)}

happyReduce_81 = happySpecReduce_3  22# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 35# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TI happy_dollar_dollar) -> cont 32#;
	PT _ (TD happy_dollar_dollar) -> cont 33#;
	PT _ (T_Id happy_dollar_dollar) -> cont 34#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut26 x))

pFunc tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut27 x))

pArgument tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut28 x))

pArgs tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut29 x))

pBody tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut30 x))

pListArgs tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut31 x))

pListFunc tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut32 x))

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut33 x))

pListId tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut34 x))

pStm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut35 x))

pListStm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut36 x))

pExp7 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut37 x))

pExp6 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut38 x))

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut39 x))

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut40 x))

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut41 x))

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut42 x))

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut43 x))

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut44 x))

pListExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut45 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}





































































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList




















infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          

          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

