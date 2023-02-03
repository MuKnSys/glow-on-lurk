const { expect } = require('chai');
var chai = require('chai')
  , chaiHttp = require('chai-http');
chai.use(chaiHttp);
var shell = require('shelljs');
const config = require('../config')
const {ChildProcess, spawn, spawnSync} = require("child_process")
const mocha = require ("mocha");
const { stderr, stdout } = require("process");
const contractPath = `${config.dapssDir}/div.glow`
const runGlowPath = `${config.runGlowDir}/run-glow`
const lurkMockServerPath = `${config.lurkConsensusMockDir}/lurk-consensus-mock`
const contractParams = config.contractParams
const contractPtcps = config.contractPtcps
const silent = config.silentMode
var server = null
var address = null
describe("Dividing", () => {
    before(function(done) {
        setTimeout(done, 1000);
    });
    beforeEach(function(done) {
        setTimeout(done, 500);
    });
    it("start lurk mock server", function(done){
        
        server = spawn(`cd ; ${lurkMockServerPath} >> /dev/pts/1`, {
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
       chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res, "Response code should be 200").to.have.status(200);
            done()
        })
    })
    it("load demo values on server", function(done) {
        chai.request('localhost:3000')
        .get('/demo/loadJM')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res, "Response code should be 200").to.have.status(200);
            expect(res.text, "Failed to load demo values").to.equal('done')
        done()
        })
    })
    it("checking is state correct", function(done){
        chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(String(res.text), "Incorrect values").to.equal('{"_accounts":{"Jan":{"_balance":100},"Marcin":{"_balance":200}},"_contracts":{}}')
            done()
        })
    })
    it("deploy contract", function(done){
        address = shell.exec(`cd ; ${runGlowPath} deploy-cli ${contractPath} ${contractParams} ${contractPtcps} `, {silent: silent}).stdout
        done()
     })
    it("checking contract on server ", function(done){
        chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            const contract = String(Object.keys(res.body._contracts));
            expect(res, "Response code should be 200").to.have.status(200);
            address = address.slice(0,-1)
            console.log(address)
            expect(contract, "Incorrect contract address").to.equal(address)
            done()
        })
    })    
    it("#1 interaction call from user A, dividing", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent: silent})
        console.log(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `)
        expect(interactionA.stdout.slice(-10, -1), `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent')
        done()
    })
    it("#1 interaction call from user B, dividing", function(done){
        const interactionB = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} "Marcin" "B" ${contractParams}  "" `, {silent: silent})
        expect(interactionB.stdout.slice(-10, -1), `Cannot interact with consenus: ${interactionB.stderr}`).to.equal('Call sent')
        done()
    })
    it("#2 interaction call from user A, dividing", function(done){
        const interactionA = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} Jan A ${contractParams}  "" `, {silent: silent})
        expect(interactionA.stdout.slice(-10, -1), `Cannot interact with consenus: ${interactionA.stderr}`).to.equal('Call sent')
        done()
    })
    it("#2 interaction call from user B, dividing", function(done){
        const interactionB = shell.exec(`cd ; ${runGlowPath} interact-cli ${contractPath}  ${address} "Marcin" "B" ${contractParams}  "" `, {silent: silent})
        expect(interactionB.stdout.slice(-10, -1), `Cannot interact with consenus: ${interactionB.stderr}`).to.equal('Call sent')
        done()
    })

    it('checking result of dividing', function(done) {
        chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            var result = Object.values(res.body._accounts)
            var balanceA = 110
            var balanceB = 190
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