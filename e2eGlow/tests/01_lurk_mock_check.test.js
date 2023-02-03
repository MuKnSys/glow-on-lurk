const { expect } = require('chai');
var chai = require('chai')
  , chaiHttp = require('chai-http');
chai.use(chaiHttp);
const {exec, ChildProcess, spawn} = require("child_process");
const config = require('../config');
const mocha = require ("mocha");
const { stderr } = require("process");
const lurkMockServerPath = `${config.lurkConsensusMockDir}/lurk-consensus-mock`;
const time = config.beforeTime
const timeEach = config.beforeEachTime

var server = null

describe("Only server", () => {
    before(function(done) {
        setTimeout(done, time);
    });
    beforeEach(function(done) {
        setTimeout(done, timeEach);
    });
    it("start lurk mock server", function( done){
        server = spawn(`cd ; ${lurkMockServerPath}`, {
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
       const request = chai.request('localhost:3000')
        .get('/state')
        .end(function (err, res){
            expect(res,"Response code should be 200").to.have.status(200);
            done()
        })
        
    })
    it("killing the server", function(done){
        var exitCode = process.kill(-server.pid);
        expect(exitCode, "Unable to kill server").to.equal(true)
        done()
    })
})
