function DataTable(conf) {
    this.conf = {
        height: conf.height || 600,
        width: conf.width || 400,
        offsetX: conf.offsetX || 0,
        offsetY: conf.offsetY || 0,
        cvsobj: conf.canvasobj,
        cvsctx: conf.canvasctx,
        bgcolor: conf.bgcolor || '#000000',
        bgalpha: conf.bgalpha || 0.8,
        cols: conf.cols,
        rowheight: conf.rowheight || 40,
        titlestyle: conf.titlestyle || '20px Arial',
        titlecolor: conf.titlecolor || '#ffffff',
        cellstyle: conf.cellstyle || '16px Arial',
        cellcolor: conf.cellcolor || '#ffffff',
        titlebgcolor: conf.titlebgcolor || '#aaaaaa',
        databgcolor1: conf.databgcolor1 || '#333333',
        databgcolor2: conf.databgcolor2 || '#555555',
        selectedcolor: conf.selectedcolor || '#99dd77',
        highlightcolor: conf.highlightcolor || '#777777',
        cellxspacing: conf.cellxspacing || 0,
        cellxpadding: conf.cellxpadding || 5,
        cellyspacing: conf.cellyspacing || 0,
        cellypadding: conf.cellypadding || 5
    };

    if (!conf.cols) {
        console.log("todo: auto col config.");
        return;
    }

    this.col_def = conf.cols;

    this.ourleftx = this.conf.offsetX;
    this.ourrightx = this.conf.width+this.conf.offsetX;

    if (this.conf.offsetY<0) {
        this.ourbottomy = this.conf.offsetY+this.conf.cvsobj.height;
        this.ourtopy = this.ourbottomy-this.conf.height;
    } else {
        this.ourtopy = this.conf.offsetY;
        this.ourbottomy = this.conf.offsetY+this.conf.height;
    }

    console.log("this dt ranges are x",this.ourleftx,this.ourrightx,
                "and y",this.ourtopy,this.ourbottomy);

    function onClick(e) {
        var element = this.conf.cvsobj;
        var offsetX = 0, offsetY = 0;

        if (element.offsetParent) {
            do {
                offsetX += element.offsetLeft;
                offsetY += element.offsetTop;
            } while ((element = element.offsetParent));
        }

        var x = e.pageX - offsetX;
        var y = e.pageY - offsetY;

        console.log("click fn: "+x+", "+y,e);
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

    this.mouseocbs=[];

    this.register_mouseo_cb = function(fn) {
        this.mouseocbs.push(fn);
    };

    this.fire_mouseo_cbs = function(argtopass) {
        var i;
        var thiscb;
        for (i=0;i<this.mouseocbs.length;i++) {
            thiscb=this.mouseocbs[i];

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

            var clickx=e.clientX-lx;
            var clicky=e.clientY-ty;
            var rowclicked=Math.floor(clicky/us.conf.rowheight)-1;

            if ((rowclicked >= 0) &&
                (rowclicked < us.datalen)) {

                var i;
                for (i=0;i<us.datalen;i++) {
                    if (i==rowclicked) {
                        us.data[i]["selected"]=true;
                    } else {
                        us.data[i]["selected"]=false;
                    }
                }

                us.render();

                us.fire_click_cbs(us.data[rowclicked]);
            }
        }
    }, false);

    this.conf.cvsobj.addEventListener('mousemove', function(e) {
        var i;

        if ((lx <= e.clientX) &&
            (rx => e.clientX) &&
            (ty <= e.clientY) &&
            (by => e.clientY)) {

            if ((e.clientX>rx)||
                (e.clientY>by)||
                (e.clientY>(ty+((1+us.datalen)*us.conf.rowheight)))) {
                // todo: figure out why above is broken
                // pt is outside

                if (us.mouseinside) {
                    for (i=0;i<us.datalen;i++) {
                        us.data[i]["highlighted"]=false;
                    }

                    us.render();
                    us.mouseinside=false;

                    us.fire_mouseo_cbs({t:'mouseoff'});
                }

                return;
            }

            us.mouseinside=true;

            var clickx=e.clientX-lx;
            var clicky=e.clientY-ty;
            var rowclicked=Math.floor(clicky/us.conf.rowheight)-1;

            if ((rowclicked >= 0) &&
                (rowclicked < us.datalen)) {

                for (i=0;i<us.datalen;i++) {
                    if (i==rowclicked) {
                        us.data[i]["highlighted"]=true;
                    } else {
                        us.data[i]["highlighted"]=false;
                    }
                }

                us.render();

                us.fire_mouseo_cbs({t:'mouseon', r: rowclicked, d:us.data[rowclicked]});
            }
        } else {
            if (us.mouseinside) {
                for (i=0;i<us.datalen;i++) {
                    us.data[i]["highlighted"]=false;
                }

                us.render();
                us.mouseinside=false;

                us.fire_mouseo_cbs({t:'mouseoff'});
            }
        }
    }, false);

    this.datalen=0;
    this.data=[];
    this.calculatedheight=false;

    this.setdata = function(robj) {
        var data=robj.data;

        var k;
        for (k=0;k<data.length;k++) {
            if (!data[k]["selected"]) data[k].selected=false;
            data[k]["highlighted"]=false;
        }
        this.data=data;
        this.datalen=data.length;


        if (this.conf.height=='auto') {


            this.calculatedheight=this.conf.rowheight*(1+this.datalen);

            console.log("attempting to autoconfigure height..... ",
                        this.calculatedheight);

            if (this.conf.offsetY<0) {
                this.ourbottomy = this.conf.offsetY+this.conf.cvsobj.height;
                this.ourtopy = this.ourbottomy-this.calculatedheight;
            } else {
                this.ourtopy = this.conf.offsetY;
                this.ourbottomy = this.conf.offsetY+this.calculatedheight;
            }

            ty=this.ourtopy;
            by=this.ourbottomy;
        }
    };

    this.render = function() {
        var data=this.data;

        var ctx=this.conf.cvsctx;

        ctx.clearRect(this.ourleftx,this.ourtopy,
                      this.ourrightx-this.ourleftx,
                      this.ourbottomy-this.ourtopy);

        ctx.globalAlpha=this.conf.bgalpha;
        ctx.fillStyle = this.conf.bgcolor;
        /*ctx.fillRect(this.ourleftx,this.ourtopy,
                     this.ourrightx-this.ourleftx,
                     this.ourbottomy-this.ourtopy);*/


        var i,d,c;
        var numcols=this.conf.cols.length;
        var rowy=this.ourtopy;
        var rowxstart=this.ourleftx;
        var colwidth=(this.ourrightx-this.ourleftx)/numcols;
        var drow=this.conf.rowheight;

        if (0==this.conf.cellxspacing) {
            ctx.fillStyle = this.conf.titlebgcolor;
            ctx.fillRect(rowxstart,rowy,this.ourrightx-this.ourleftx,
                         this.conf.rowheight);
        }

        for (c=0;c<numcols;c++) {
            var cold=this.conf.cols[c];

            var cx=rowxstart+(colwidth*c);
            var cy=rowy;
            var dx=colwidth;
            var dy=drow;

            if (0!=this.conf.cellxspacing) {
                ctx.fillStyle = this.conf.titlebgcolor;
                ctx.fillRect(cx+this.conf.cellxspacing,
                             cy+this.conf.cellyspacing,
                             dx-this.conf.cellxspacing,
                             dy-this.conf.cellyspacing);
            }

            ctx.font = this.conf.titlestyle;
            ctx.fillStyle = this.conf.cellcolor;
            ctx.fillText(cold.title,cx+this.conf.cellxpadding,
                         cy+drow-this.conf.cellypadding);
        }
        rowy+=drow;

        for (i=0;i<data.length;i++) {
            d=data[i];

            var thisbgcolor=(0==(i%2))?this.conf.databgcolor1:this.conf.databgcolor2;

            if (d.highlighted) thisbgcolor=this.conf.highlightcolor;
            if (d.selected) thisbgcolor=this.conf.selectedcolor;


            if (0==this.conf.cellxspacing) {
                ctx.fillStyle = thisbgcolor;
                ctx.fillRect(rowxstart,rowy,this.ourrightx-this.ourleftx,
                             this.conf.rowheight);
            }

            for (c=0;c<numcols;c++) {
                var colv=d[this.conf.cols[c].key];

                var cx=rowxstart+(colwidth*c);
                var cy=rowy;
                var dx=colwidth;
                var dy=drow;

                if (0!=this.conf.cellxspacing) {
                    ctx.fillStyle = thisbgcolor;
                    ctx.fillRect(cx+this.conf.cellxspacing,
                                 cy+this.conf.cellyspacing,
                                 dx-this.conf.cellxspacing,
                                 dy-this.conf.cellyspacing);
                }

                ctx.font = this.conf.cellstyle;
                ctx.fillStyle = this.conf.cellcolor;
                ctx.fillText(colv,cx+this.conf.cellxpadding,
                             cy+drow-this.conf.cellypadding);

            }
            rowy+=drow;
        }
    };
}
