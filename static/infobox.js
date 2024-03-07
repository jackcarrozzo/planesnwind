function InfoBox(conf) {
    this.conf = {
        height: conf.height || 300,
        width: conf.width || 600,
        offsetX: conf.offsetX || 0,
        offsetY: conf.offsetY || 0,
        cvsobj: conf.canvasobj,
        cvsctx: conf.canvasctx,
        titleheight: conf.titleheight || 60,
        titlebgcolor: conf.titlebgcolor || '#000000',
        titlebgalpha: conf.titlebgalpha || 0.8,
        titlestyle: conf.titlestyle || '20px Arial',
        titlecolor: conf.titlecolor || '#ffffff',
        textbgcolor: conf.textbgcolor || '#000000',
        textbgalpha: conf.textbgalpha || 0.8,
        textstyle: conf.textstyle || '16px Arial',
        textcolor: conf.textcolor || '#ffffff',
        padding: conf.padding||10
    };

    this.ourleftx = this.conf.offsetX;
    this.ourrightx = this.conf.width+this.conf.offsetX;

    if (this.conf.offsetY<0) {
        this.ourbottomy = this.conf.offsetY+this.conf.cvsobj.height;
        this.ourtopy = this.ourbottomy-this.conf.height;
    } else {
        this.ourtopy = this.conf.offsetY;
        this.ourbottomy = this.conf.offsetY+this.conf.height;
    }

    var lx=this.ourleftx;
    var ty=this.ourtopy;
    var rx=this.ourrightx;
    var by=this.ourbottomy;
    var us=this;
    this.mouseinside=false;

    this.clickcbs=[];

    this.register_click_cb = function(fn) {
        this.clickcbs.push(fn);
    };

    this.fire_click_cbs = function(argtopass) {
        var i;
        var thiscb;
        for (i=0;i<this.clickcbs.length;i++) {
            thiscb=this.clickcbs[i];

            thiscb(argtopass);
        }
    };

    this.conf.cvsobj.addEventListener("click", function(e) {
        if ((lx <= e.clientX) &&
            (rx => e.clientX) &&
            (ty <= e.clientY) &&
            (by => e.clientY)) {

            if ((e.clientX>rx)||
                (e.clientY>by)) {
                // todo: figure out why above is broken
                // pt is outside
                return;
            }

            us.fire_click_cbs();
        }

    }, false);

    this.title="";
    this.text="";

    this.setdata = function(robj) {
        this.title=robj.title||"";
        this.text=robj.text||"";
    };

    this.hidden=true;
    this.hide = function() {
        var ctx=this.conf.cvsctx;

        this.hidden=true;
        ctx.clearRect(this.ourleftx,this.ourtopy,
                      this.ourrightx-this.ourleftx,
                      this.ourbottomy-this.ourtopy);
    };

    this.show = function() {
        this.hidden=false;
        this.render();
    };

    this.render = function() {
        if (this.hidden) return;

        var ctx=this.conf.cvsctx;
        var txt=this.text;

        ctx.clearRect(this.ourleftx,this.ourtopy,
                      this.ourrightx-this.ourleftx,
                      this.ourbottomy-this.ourtopy);

        ctx.globalAlpha=this.conf.titlebgalpha;
        ctx.fillStyle = this.conf.titlebgcolor;

        ctx.fillRect(this.ourleftx,this.ourtopy,
                     this.ourrightx-this.ourleftx,
                     this.conf.titleheight);

        ctx.font = this.conf.titlestyle;
        ctx.fillStyle = this.conf.titlecolor;
        ctx.fillText(this.title,this.ourleftx+this.conf.padding,this.ourtopy+this.conf.titleheight-this.conf.padding);

        function wraptext(ctx,text,x,y,maxwidth,lineheight) {
            var words = text.split(' ');
            var line = '';

            for(var n=0;n<words.length;n++) {
                var testLine = line + words[n] + ' ';
                var metrics = ctx.measureText(testLine);
                var testWidth = metrics.width;
                if (testWidth > maxwidth && n > 0) {
                    ctx.fillText(line, x, y);
                    line = words[n] + ' ';
                    y += lineheight;
                }
                else {
                    line = testLine;
                }
            }
            ctx.fillText(line, x, y);
        }

        ctx.globalAlpha=this.conf.textbgalpha;
        ctx.fillStyle = this.conf.textbgcolor;

        ctx.fillRect(this.ourleftx,this.ourtopy+this.conf.titleheight,
                     this.ourrightx-this.ourleftx,
                     this.ourbottomy-(this.ourtopy+this.conf.titleheight));

        var lineheight=20;
        ctx.font = this.conf.textstyle;
        ctx.fillStyle = this.conf.textcolor;
        wraptext(ctx,txt,this.ourleftx+this.conf.padding,
                 this.ourtopy+this.conf.titleheight+this.conf.padding+lineheight,
                 ((this.ourrightx-this.ourleftx)-(2*this.conf.padding)),
                 lineheight);

    };
}
