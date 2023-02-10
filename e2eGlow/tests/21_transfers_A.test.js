const { expect } = require('chai');
var chai = require('chai')
  , chaiHttp = require('chai-http');
chai.use(chaiHttp);
var shell = require('shelljs');
const config = require('../config')
const {spawn} = require("child_process")
const contractPath = `${config.dapssDir}/transfer_A.glow`
const runGlowPath = `${config.runGlowDir}/run-glow`
const lurkMockServerPath = `${config.lurkConsensusMockDir}/lurk-consensus-mock`
const contractParams = config.contractParamsNoEscrow
const contractPtcps = config.contractPtcps
const silent = config.silentMode
var server = null
var address = null
const time = config.beforeTime;
const timeEach = config.beforeEachTime;
const serverAddress = config.serverAddress;

describe("Transfering founds between one participant and consensus", () => {
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
            expect(data, "Error while trying to run mock server").to.be.null;
        }); 
        done()
    })
    it("server check", function(done){
       chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res).to.have.status(200);
            done()
        })
    })
    it("load demo values on server", function(done) {
        chai.request(serverAddress)
        .get('/demo/loadJM')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res).to.have.status(200);
            expect(res.text).to.equal('done')
            done()
        })
    })
    it("checking is state correct", function(done){
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(String(res.text)).to.equal('{"_accounts":{"Jan":{"_balance":100},"Marcin":{"_balance":200}},"_contracts":{}}')
            done()
        })
    })
    it("deploy contract", function(done){
        address = shell.exec(`cd ; ${runGlowPath} deploy-cli ${contractPath} ${contractParams} ${contractPtcps} `, {silent : silent}).stdout
        done()
     })
    it("checking contract on server ", function(done){
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            const contract = String(Object.keys(res.body._contracts));
            expect(res).to.have.status(200);
            address = address.slice(0,-1)
            expect(contract).to.equal(address)
            done()
        })
    })    
    it("#1 interaction call from user A, deposit", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent : silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    })
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 90
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("#2 interaction call from user A, withdraw", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" ` , {silent : silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    })
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 100
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("#3 interaction call from user A, deposit", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent : silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    })
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 90
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("#4 interaction call from user A, deposit", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" ` , {silent :silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    })
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 80
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("#5 interaction call from user A, withdraw", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent : silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    })
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 90
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("#6 interaction call from user A, deposit", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent : silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    })
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 80
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("#7 interaction call from user A, withdraw", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent : silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    }) 
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 90
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("#8 interaction call from user A, withdraw", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent:silent})
        expect(interactionA.stdout, `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent\n')
        done()
    })
    it('checking result of transfer', function(done) {
        chai.request(serverAddress)
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 100
            var balanceB = 200
            expect(JSON.stringify(result)).to.equal(`[{"_balance":${balanceA}},{"_balance":${balanceB}}]`)
            done()
        })
    })
    it("killing the server", function(done){
        var exitCode = process.kill(-server.pid);
        expect(exitCode).to.equal(true)
        done()
    })
}) 