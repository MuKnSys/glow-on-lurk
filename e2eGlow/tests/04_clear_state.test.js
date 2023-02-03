const { expect } = require('chai');
var chai = require('chai')
  , chaiHttp = require('chai-http');
chai.use(chaiHttp);
const {exec, ChildProcess, spawn} = require("child_process")
var shell = require('shelljs');
const config = require('../config')
const mocha = require ("mocha");
const { stderr, stdout } = require("process");
const lurkMockServerPath = config.lurkConsensusMockDir
var server = null
var address = null


describe("Removing contract from state", () => {
    before(function(done) {
        setTimeout(done, 1000);
    });
    beforeEach(function(done) {
        setTimeout(done, 500);
    });
    it("start lurk mock server", function(done){
        server = spawn(`cd ; ${lurkMockServerPath}/lurk-consensus-mock`, {
            shell: true,
            detached: true
        });
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
            expect(res,"Response code should be 200").to.have.status(200);
            done()
        })
    })
    it("checking contract on server ", function(done){
        chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            contract = String(Object.keys(res.body));
            expect(res,"Response code should be 200").to.have.status(200);
            expect(contract,"There is no contract on server").to.exist
            done()
        })
    })
    it("clear server state", function(done){
        chai.request('localhost:3000')
        .get('/clearState')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res,"Response code should be 200").to.have.status(200);
            done()
        })
    })
    it("checking contract on server ", function(done){
        chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res).to.have.status(200);
            expect(res.text, "Unable to clear server state").to.equal('{"_accounts":{},"_contracts":{}}')
            done()
        })
    })
    it("killing the server", function(done){
        var exitCode = process.kill(-server.pid);
        expect(exitCode, "Unable to kill server").to.equal(true)
        done()
    })
})
