const { expect } = require('chai');
var chai = require('chai')
  , chaiHttp = require('chai-http');
chai.use(chaiHttp);
var shell = require('shelljs');
const config = require('../config')
const {spawn} = require("child_process")
const contractPath = `${config.dapssDir}/multiply.glow`
const runGlowPath = `${config.runGlowDir}/run-glow`
const lurkMockServerPath = `${config.lurkConsensusMockDir}/lurk-consensus-mock`
const contractParams = config.contractParams
const contractPtcps = config.contractPtcps
const silent = config.silentMode
var server = null
var address = null
const time = config.beforeTime;
const timeEach = config.beforeEachTime;
const serverAddress = config.serverAddress;

describe("Multiplication", () => {
    before(function(done) {
        setTimeout(done, time);
    });
    beforeEach(function(done) {
        setTimeout(done, timeEach);
    });
    it("start lurk mock server", function(done){
        server = spawn(`${lurkMockServerPath}`, {
            shell: true,
            detached: true
        })
        server.stderr.on('data', (data) => {
            console.log(`data: ${data}`);
            expect(data, "Error while trying to run new mock server").to.be.null;
        });
        done()
    })
    it("server check", function(done){
       chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res, "Response code should be 200").to.have.status(200);
            done()
        })
    })
    it("load demo values on server", function(done) {
        chai.request(serverAddress)
        .get('/demo/loadJM')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res, "Response code should be 200").to.have.status(200);
            expect(res.text, "Failed to load demo values").to.equal('done')
        done()
        })
    })
    it("checking is state correct", function(done){
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(String(res.text), "Incorrect values").to.equal('{"_accounts":{"Jan":{"_balance":100},"Marcin":{"_balance":200}},"_contracts":{}}')
            done()
        })
    })
    it("deploy contract", function(done){
        address = shell.exec(`${runGlowPath} deploy-cli ${contractPath} ${contractParams} ${contractPtcps} `, {silent : silent}).stdout
        done()
    })
    it("checking contract on server ", function(done){
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            const contract = String(Object.keys(res.body._contracts));
            expect(res, "Response code should be 200").to.have.status(200);
            address = address.slice(0,-1)
            expect(contract, "Incorrect contract address").to.equal(address)
            done()
        })
    })    
    it("#1 interaction call from user A", function(done){
        const interactionA = shell.exec(`${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent: silent})
        expect(interactionA.stdout.slice(-10, -1), `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent')
        done()
    })
    it("#1 interaction call from user B", function(done){
        const interactionB = shell.exec(`${runGlowPath} interact-cli ${contractPath}  ${address} Marcin B ${contractParams}  "" `, {silent : silent})
        expect(interactionB.slice(-10, -1), `Cannot interact with consenus: ${interactionB.stderr}`).to.equal('Call sent')
        done()
    })
    it("#2 interaction call from user A", function(done){
        const interactionA = shell.exec(`${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent : silent})
        expect(interactionA.stdout.slice(-10, -1), `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent')
        done()
    })
    it("#2 interaction call from user B", function(done){
        const interactionB = shell.exec(`${runGlowPath} interact-cli ${contractPath}  ${address} Marcin B ${contractParams}  "" `, {silent : silent})
        expect(interactionB.stdout.slice(-10, -1), `Cannot interact with consenus: ${interactionB.err}`).to.equal('Call sent')
        done()
    })
    it('checking result of interaction', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 140
            var balanceB = 160
            expect(JSON.stringify(result), "Incorrect result value").to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("killing the server", function(done){
        var exitCode = process.kill(-server.pid);
        expect(exitCode, "Unable to kill server").to.equal(true)
        done()
    })
})