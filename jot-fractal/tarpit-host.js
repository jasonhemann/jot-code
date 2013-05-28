var Tarpit = new Object();
var CurrentImage = undefined;

function handleMessage(m) {
    var m = m.data;

    switch(m.cmd) {
        case 'progress':
        var p = document.getElementById('render_progress');
        p.style.width = "" + m.percent + "%";
        break;

        case 'complete':
        CurrentImage = m;
        render(m);
        break;
    }
}

function startup() {
    Tarpit.worker = new Worker('tarpit.js');
    Tarpit.worker.addEventListener('message', handleMessage);
    //Tarpit.worker.postMessage({cmd: 'serve', num_workers: 8});
    compute();
}

function compute() {
    var p = document.getElementById('render_progress');
    p.style.width = 0;

    var res = parseInt(document.getElementById('resolution').value);
    var limit = 200;
    Tarpit.worker.postMessage({cmd: 'render', 'res': res, 'limit': limit});
}

function render(m) {
    var c = document.getElementById('#tarpit');
    var ctx = c.getContext("2d");
    
    var limit = m.limit;
    var N = 1 << m.res;
    var w = c.width / N;

    for(var i = 0; i < N; i++) {
        for(var j = 0; j < N; j++) {
            var c = m.data[i * N + j];

            if(c < limit) {
                c /= limit;
                //c = Math.log(c + 1);
                //c = Math.log(c + 1) / log2;
                c = Math.floor(c * 256);
                ctx.fillStyle = "rgb(" + c + ", " + c + ", " + c + ")";
            }
            else {
                ctx.fillStyle = "#FF0000";
            }
            
            ctx.fillRect(i * w, j * w, w + 1, w + 1);
            
        }
    }
}

startup();
