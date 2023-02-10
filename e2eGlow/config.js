const config = {
    dapssDir : __dirname + '/contracts',
    runGlowDir: '$HOME/Desktop/glowOnLurk/dist-newstyle/build/x86_64-linux/ghc-9.2.4/glow-0.1.0.0/x/run-glow/build/run-glow',
    lurkConsensusMockDir: '$HOME/Desktop/glowOnLurk/dist-newstyle/build/x86_64-linux/ghc-9.2.4/glow-0.1.0.0/x/lurk-consensus-mock/build/lurk-consensus-mock',
    contractParams : `"[GLNat 10, GLNat 20]"`,
    contractSParams : `"[GLNat 10, GLNat 10]"`,
    contractParamsEqual : `"[GLNat 40, GLNat 40]"`,
    contractParamsGT : `"[GLNat 50, GLNat 50]"`,
    contractParamsLT : `"[GLNat 5, GLNat 5]"`,
    contractParamsBig : `"[GLNat 1000, GLNat 1000]"`,
    contractParamsNoEscrow : `"[GLNat 10]"`,
    contractParamsNoParams : `"[]"`,
    contractPtcps : `"[(Id {idBS = "\\\"A"\\\"},LedgerPubKey {lpkBS = "\\\"Jan"\\\"}),(Id {idBS = "\\\"B"\\\"},LedgerPubKey {lpkBS = "\\\"Marcin"\\\"})]"`,
    silentMode : true,
    beforeTime : 1000,
    beforeEachTime : 500,
    serverAddress : 'localhost:3000'

}
module.exports = config