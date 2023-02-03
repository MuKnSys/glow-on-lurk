const { expect } = require('chai');
var chai = require('chai')
  , chaiHttp = require('chai-http');
chai.use(chaiHttp);
const {exec, ChildProcess, spawn} = require("child_process")
const config = require ('../config')
const mocha = require ("mocha");
const { stderr } = require("process");
const { isGeneratorFunction } = require('util/types');
const lurkMockServerPath = `${config.lurkConsensusMockDir}/lurk-consensus-mock`
var server = null

describe("Load demo values", () => {
    before(function(done) {
        setTimeout(done, 1000);
    });
    beforeEach(function(done) {
        setTimeout(done, 500);
    });
    it("start lurk mock server", function(done){
        server = spawn(`cd ; pwd ; ${lurkMockServerPath}`, {
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
    it("load values on server", function(done) {
        chai.request('localhost:3000')
        .get('/demo/loadJM')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(res,"Response code should be 200").to.have.status(200);
            expect(res.text, "Failed to load demo values").to.equal('done')
        done()
        })
    })
    it("checking is state correct", function(done){
        chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(err).to.be.null;
            expect(String(res.text),"Incorrect values").to.equal('{"_accounts":{"Jan":{"_balance":100},"Marcin":{"_balance":200}},"_contracts":{}}')
            done()
        })
    })
    it("killing the server", function(done){
        var exitCode = process.kill(-server.pid);
        expect(exitCode, "Unable to kill server").to.equal(true)
        done()
    })

})




