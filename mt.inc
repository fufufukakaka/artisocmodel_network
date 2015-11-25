' ＶＢＡによるメルセンヌツイスタ


' メルセンヌツイスタのパラメータ(ダイナミッククリエーターの結果)
Private Const MTN = 644, MTM = 322, MTA = 12, MTB = 7, MTC = 15, MTD = 18
Private Const MXA = &H70C20000, UMK = &H78000000, LMK = &H7FFFFFF
Private Const MKB = &H73736B80, MKC = &H6ED28000


' 補助的な定数の宣言
Private Const MTL = MTN - MTM, MTK = MTN - 1, MTJ = MTL - 1, MTP = MTN - 2
Private Const PWA = 2 ^ MTA, PWB = 2 ^ MTB, PWC = 2 ^ MTC, PWD = 2 ^ MTD
Private Const KB = MKB \ PWB, KC = MKC \ PWC
Private Const P32 = 2# ^ 32, P31 = 2 ^ 31, P22 = 2# ^ 22, P9 = 2 ^ 9
Private Const M53 = 2# ^ -53, M32 = 2# ^ -32, M30 = 2# ^ -30


' 乱数の状態
Private mt(0 To MTK), mti As Long


' 初期化の補助関数
Function Ri(ByRef r As Double, ByVal i As Long) As Long
{
    Dim s As Variant
    Dim shft As Double
    Dim a As Long
    If r >= P31 Then a = r - P32 Else a = r
    a = a Xor Int(r * M30)
    If a < 0 Then r = a + P32 Else r = a
    s = 1812433253 * CDec(r) + i: r = s - CDec(Int(s * M32)) * P32
    If r >= P31 Then Ri = r - P31 Else Ri = r
}


' s を種にして乱数を初期化する
Sub InitMt(ByVal s As Long)
{
    Dim r As Double
    mt(0) = s And &H7FFFFFFF
    If s < 0 Then r = P32 + s Else r = s
    For mti = 1 To MTK: mt(mti) = Ri(r, mti): Next mti
    mti = MTN
}


' 31 ビットの整数乱数
Function NextMt() As Long
{
    Dim y, k As Long
    If mti = 0 Then InitMt (1)
    If mti = MTN Then
        mti = 0
        For k = 0 To MTJ
            y = (mt(k) And UMK) Or (mt(k + 1) And LMK)
            mt(k) = mt(k + MTM) Xor (y \ 2) Xor (-(y And 1) And MXA)
        Next k
        For k = MTL To MTP
            y = (mt(k) And UMK) Or (mt(k + 1) And LMK)
            mt(k) = mt(k - MTL) Xor (y \ 2) Xor (-(y And 1) And MXA)
        Next k
        y = (mt(MTK) And UMK) Or (mt(0) And LMK)
        mt(MTK) = mt(MTM - 1) Xor (y \ 2) Xor (-(y And 1) And MXA)
    End If
    y = mt(mti): mti = mti + 1
    y = y Xor (y \ PWA): y = y Xor ((y And KB) * PWB)
    y = y Xor ((y And KC) * PWC): y = y Xor (y \ PWD): NextMt = y
}


' 0 以上 1 未満の乱数を返す
Function NextUnifMt() As Double
{
    Dim x As Long
    x = NextMt \ P9: NextUnifMt = (NextMt * P22 + x) * M53
}


' 時間を種にして乱数を初期化する
Sub RandomizeMt()
{
    InitMt (GetRealTime())
}