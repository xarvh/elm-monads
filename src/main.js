const elm = require('../build/app')

const elmApp = elm.Monad.worker(process.argv)

elmApp.ports.printAndExit.subscribe(function (string) {
    console.info(string);
    process.exit(0);
});
