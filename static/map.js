function FlightMap(conf) {
    this.conf = {
        divid: conf.id || "map0",
        height: conf.height || (window.innerHeight),
        width: conf.width || (window.innerWidth),
        style:  conf.style  || null,
        dir: conf.dir || "vertical",
        bgcolor: conf.bgcolor || "#444"
    };

    this.c_keys = ['apts', 'winds', 'paths', 'selpaths', 'ui'];
    this.winwidth=-1;
    this.winheight=-1;
    this.windmodel = null;
    this.bounding_area_set = false;

    var us=this;

    this.setup = function() {
        this.divobj = document.getElementById(this.conf.divid);
        if (!this.divobj) { console.log("missing div:",this.conf.divid); return; }

        var i,c_key;
        var divstr="";
        for (i=0;i<this.c_keys.length;i++) {
            c_key=this.c_keys[i];
            divstr+="<canvas id='"+this.conf.divid+"_canvas_"+c_key+"'></canvas>";
        }
        this.divobj.innerHTML = divstr;

        this.winwidth=this.conf.width;
        this.winheight=this.conf.height;

        this.canvasobjs = {};
        this.canvasctxs = {};
        this.canvasz = {};

        for (i=0;i<this.c_keys.length;i++) {
            c_key=this.c_keys[i];

            this.canvasobjs[c_key] = document.getElementById(this.conf.divid+
                                                             "_canvas_"+c_key);
            this.canvasctxs[c_key] = this.canvasobjs[c_key].getContext("2d");

        }

        if (this.conf.style) {
            console.log("map obj requested a style but we ignored it.");
        }

        for (i=0;i<this.c_keys.length;i++) {
            c_key=this.c_keys[i];

            this.canvasobjs[c_key].setAttribute("width", this.winwidth);
            this.canvasobjs[c_key].setAttribute("height", this.winheight);
            this.canvasz[c_key] =  "z-index: "+(i+1)+";";
            this.canvasobjs[c_key].setAttribute("style", this.canvasz[c_key]);

            /*this.canvasobjs[c_key].addEventListener('mousemove', function(evt) {
                //var mousePos = getMousePos(canvas, evt);
                //var message = 'Mouse position: ' + mousePos.x + ',' + mousePos.y;
                //writeMessage(canvas, message);
                console.log(c_key+": "+evt.clientX+", "+evt.clientY);
            }, false);*/
        }

        this.timerid=setInterval(function(){
            if ((us.winwidth!=window.innerWidth)||
                (us.winheight!=window.innerHeight)) {

                us.clearall(us);

                us.winwidth=window.innerWidth;
                us.winheight=window.innerHeight;

                var i;
                for (i=0;i<us.c_keys.length;i++) {
                    c_key=us.c_keys[i];

                    us.canvasobjs[c_key].setAttribute("width", us.winwidth);
                    us.canvasobjs[c_key].setAttribute("height", us.winheight);
                }

                us.render_airports(null);
                us.render_winds(null);
                us.render_flights(null);
                us.render_sel_paths(null);

                us.fire_resize_cbs(us);
            }
        }, 1000);
    };

    this.resizecbs=[];

    this.register_resize_cb = function(fn) {
        this.resizecbs.push(fn);
    };

    this.fire_resize_cbs = function(argtopass) {
        var i;
        var thiscb;
        for (i=0;i<this.resizecbs.length;i++) {
            thiscb=this.resizecbs[i];

            thiscb(argtopass);
        }
    };

    this.set_stats = function(s) {
        this.overall=s.overall;
        this.constrained=s.constrained;
        this.numfls=s.num;
        this.numconstrfls=s.cnum;

        this.brag();
    };

    this.brag = function() {
        var ctx=this.canvasctxs['ui'];

        ctx.font = '200px Garamond';
        ctx.fillStyle = '#000000';
        ctx.fillText(Math.round(100*this.constrained)/100,this.winwidth-500,this.winheight-400);

        ctx.font = '100px Garamond';
        ctx.fillText("%",this.winwidth-150,this.winheight-400);

        ctx.font = '72px Garamond';
        ctx.fillText("Avg distance constrained improvement:",this.winwidth-1700,this.winheight-400);

        ctx.font = '200px Garamond';
        ctx.fillStyle = '#000000';
        ctx.fillText(Math.round(100*this.overall)/100,this.winwidth-500,this.winheight-200);

        ctx.font = '100px Garamond';
        ctx.fillText("%",this.winwidth-150,this.winheight-200);

        ctx.font = '72px Garamond';
        ctx.fillText("Avg overall improvement:",this.winwidth-1310,this.winheight-200);

        ctx.font = '100px Garamond';
        ctx.fillStyle = '#000000';
        ctx.fillText(this.numfls,this.winwidth-500,this.winheight-100);

        ctx.font = '60px Garamond';
        ctx.fillText("flights",this.winwidth-200,this.winheight-100);

        ctx.font = '60px Garamond';
        ctx.fillText("across",this.winwidth-700,this.winheight-100);
    };

    this.clearall = function() {
        var c;
        for (c in us.c_keys) {
            us.clear_layer(us.c_keys[c]);
        }
    };

    this.clear_layer = function(lkey) {
        var ctx=us.canvasctxs[lkey];
        ctx.clearRect(0,0, us.winwidth, us.winheight);
    };

    this.hide_layer = function(c_key) {
        var c=us.canvasobjs[c_key];
        if (c) {
            c.setAttribute("style", "z-index: 0;");
        }
    };

    this.show_layer = function(c_key) {
        var c=us.canvasobjs[c_key];
        if (c) {
            c.setAttribute("style", us.canvasz[c_key]);
        }
    };

    this.x2chart = function(x) {
        return this.winwidth*(x-this.xmin)/(this.xmax-this.xmin);
    };

    this.y2chart = function(y) {
        return this.winheight-(this.winheight*
            (y-this.ymin)/(this.ymax-this.ymin));
    };

    this.deg2rad = function(d) {
        return d*Math.PI/180.0;
    };

    this.rad2deg = function(r) {
        return r*180.0/Math.PI;
    };

    this.earthrad_m = 6371e3;

    this.haversine = function(xlon0,ylat0,xlon1,ylat1) {
        var xlon0r=this.deg2rad(xlon0);
        var ylat0r=this.deg2rad(ylat0);
        var xlon1r=this.deg2rad(xlon1);
        var ylat1r=this.deg2rad(ylat1);
        var dxlonr=this.deg2rad(xlon1-xlon0);
        var dylatr=this.deg2rad(ylat1-ylat0);

        var a=Math.pow(Math.sin(dylatr/2.0),2)+
            (Math.cos(ylat0r)*Math.cos(ylat1r)*
             Math.pow(Math.sin(dxlonr/2.0),2));
        var c=2.0*Math.atan2(Math.sqrt(a), Math.sqrt(1.0-a));

        return c*this.earthrad_m;
    };

    this.gcinterp = function(xlon0,ylat0,xlon1,ylat1,d,p) {
        var xlon0r=this.deg2rad(xlon0);
        var ylat0r=this.deg2rad(ylat0);
        var xlon1r=this.deg2rad(xlon1);
        var ylat1r=this.deg2rad(ylat1);

        var sigma=d/this.earthrad_m;
        var a=Math.sin(sigma*(1.0-p))/Math.sin(sigma);
        var b=Math.sin(sigma*p)/Math.sin(sigma);
        var x=(a*Math.cos(ylat0r)*Math.cos(xlon0r))+
            (b*Math.cos(ylat1r)*Math.cos(xlon1r));
        var y=(a*Math.cos(ylat0r)*Math.sin(xlon0r))+
            (b*Math.cos(ylat1r)*Math.sin(xlon1r));
        var z=(a*Math.sin(ylat0r))+(b*Math.sin(ylat1r));

        return {
            ylat: this.rad2deg(Math.atan2(z, Math.sqrt(Math.pow(x,2)+Math.pow(y,2)))),
            xlon: this.rad2deg(Math.atan2(y,x))
        };
    };

    this.airports=-1;

    this.render_airports = function(aptobj) {
        if (aptobj) this.airports=aptobj;
        else aptobj=this.airports;

        this.xmin=aptobj.min_xlon;
        this.xmax=aptobj.max_xlon;
        this.ymin=aptobj.min_ylat;
        this.ymax=aptobj.max_ylat;
        this.bounding_area_set=true;

        var ctx = this.canvasctxs.apts;

        ctx.clearRect(0,0, this.winwidth, this.winheight);

        // todo: figure out how to properly set the bgcolor and clear rect properly
        ctx.fillStyle = this.conf.bgcolor;
        ctx.fillRect(0,0,this.winwidth,this.winheight);

        console.log("btw the bgcolor were using is",this.conf.bgcolor);

        var n=0;
        var aind;
        var thisapt;
        var thisx,thisy;
        for (aind in aptobj.airports) {
            thisapt=aptobj.airports[aind];

            thisx=this.x2chart(thisapt.lon);
            thisy=this.y2chart(thisapt.lat);

            ctx.globalAlpha=0.65;

            ctx.beginPath();
            ctx.arc(thisx, thisy, 5, 0, 2 * Math.PI, false);
            ctx.fillStyle = '#d4e5d0'; //'#e6ffcc';
            ctx.fill();
            //ctx.lineWidth = 1;
            //ctx.strokeStyle = '#bbffbb';
            //ctx.stroke();
        }
        console.log("rendered",(1+aind),"airports.");
    };

    this.windmodel_key = function(comp,xlon,ylat,alt) {
        return comp.toUpperCase()+"-"+
            Math.round(xlon)+"-"+
            Math.round(ylat)+"-"+
            Math.round(alt);
    };

    this.winds=-1;

    this.render_winds = function(windsobj) {
        if (windsobj) us.winds=windsobj;
        else windsobj=us.winds;

        var ctx = this.canvasctxs.winds;
        this.windmodel=windsobj;

        console.log("wind spd and dir keys and vals:",
                    this.windmodel_key('wspd',-70.1,35.2,39000),
                    this.windmodel_key('wdir',-70.1,35.2,39000),
                    windsobj[this.windmodel_key('wspd',-70.1,35.2,39000)],
                    windsobj[this.windmodel_key('wdir',-70.1,35.2,39000)]);

        var alttoshow=34000;

        if (!this.bounding_area_set) {
            console.log("cant render winds, bounding box unset.");
            return;
        }

        var model_keys=Object.keys(this.windmodel);
        var filter_pat=new RegExp(/34000$/);

        /*var ki;
        var this_key;
        var n=0;
        for (ki in model_keys) {
            this_key=model_keys[ki];

            if (!filter_pat.test(this_key)) continue;

            if (n++>20) break;

            // todo: compile
            var m = /^(....)-(.+)-(.+)-(.+)$/.exec(this_key);
            var this_xlon,this_ylat,this_wspd,this_wdir;

            }*/

        var this_xlon,this_ylat,this_wspd,this_wdir;
        var thisx,thisy;
        for (this_xlon=this.xmin;this_xlon<=this.xmax;this_xlon++) {
            for (this_ylat=this.ymin;this_ylat<=this.ymax;this_ylat++) {
                thisx=this.x2chart(this_xlon);
                thisy=this.y2chart(this_ylat);

                this_wspd=this.windmodel[this.windmodel_key('wspd',this_xlon,
                                                            this_ylat,alttoshow)];
                this_wdir=this.windmodel[this.windmodel_key('wdir',this_xlon,
                                                            this_ylat,alttoshow)];

                ctx.globalAlpha=0.5;

                ctx.beginPath();
                ctx.arc(thisx, thisy, 3, 0, 2 * Math.PI, false);
                ctx.fillStyle = '#90b9ce'; //'#e6ffcc';
                ctx.fill();
                ctx.lineWidth = 1;
                ctx.strokeStyle = '#90b9ce';
                ctx.stroke();

                // todo move to conf
                var mag_ratio=1.0;
                var wind_max=140.0;

                var d_mag=(this_wspd/wind_max)*
                    (this.x2chart(-81)-this.x2chart(-80));
                var wrad=this.deg2rad(90.0-this_wdir);
                var x_ep=d_mag*Math.cos(wrad);
                var y_ep=d_mag*Math.sin(wrad);

                ctx.globalAlpha=0.3;
                ctx.strokeStyle="#ff4444";
                ctx.lineWidth=2;
                ctx.beginPath();
                ctx.moveTo(thisx,thisy);
                ctx.lineTo(thisx+x_ep,thisy+y_ep);
                ctx.stroke();
            }
        }
    };

    this.deg2rad=function(d) {
        return d*Math.PI/180.0;
    };

    this.rad2deg=function(r) {
        return r*180.0/Math.PI;
    };

    this.flightpaths=-1;

    this.render_flights = function(fpobj) {
        if (fpobj) us.flights=fpobj;
        else fpobj=us.flightpaths;

        var ctx=this.canvasctxs.paths;

        var ind;
        var thispathset;
        for (ind in fpobj) {
            thispathset=fpobj[ind];

            us.render_one_path(thispathset[0], {
                color: '#90b9ce',
                alpha: 0.2,
                linewidth: 1,
                rad: 1
            });

            if (thispathset[1]&&(thispathset[1].length>10)) {
                us.render_one_path(thispathset[1], {
                    color: '#aab9ce',
                    alpha: 0.2,
                    linewidth: 1,
                    rad: 2
                });
            }

            if (thispathset[2]&&(thispathset[2].length>10)) {
                us.render_one_path(thispathset[2], {
                    color: '#ff80ce',
                    alpha: 0.2,
                    linewidth: 1,
                    rad: 1
                });
            }
        }
    };

    this.render_one_path = function(pts,conf) {
        var ctx=this.canvasctxs.paths;

        var ind,thispt;
        var thisx,thisy,lastx,lasty;
        var col=conf.color||'#ff0000';

        ctx.lineWidth = conf.linewidth||1;
        ctx.strokeStyle = col;
        ctx.fillStyle = col;
        ctx.globalAlpha=conf.alpha||0.2;

        for (ind=0;ind<pts.length;ind++) {
            thispt=pts[ind];

            thisx=this.x2chart(thispt[0]);
            thisy=this.y2chart(thispt[1]);

            ctx.beginPath();
            ctx.arc(thisx, thisy, conf.rad||1, 0, 2 * Math.PI, false);
            ctx.fill();
            ctx.stroke();
        }
    };

    this.clear_sel_paths = function() {
        var ctx=this.canvasctxs.selpaths;
        ctx.clearRect(0,0, this.winwidth, this.winheight);
    };

    this.set_paths = function(fp) {
        this.flightpaths=fp;
    };

    this.selectedid=-1;

    this.render_sel_paths = function(fid) {
        if (fid) us.selectedid=fid;
        else fid=us.selectedid;

        this.render_spartan_path({
            pts_ar: this.flightpaths[fid][0],
            lines: false,
            color: '#f2b10e',
            ptrad: 2
        });

        if (this.flightpaths[fid][1]&&(this.flightpaths[fid][1].length>0)) {
            this.render_spartan_path({
                pts_ar: this.flightpaths[fid][1],
                lines: false,
                color: '#c60fab',
                ptrad: 2
            });
        }

        if (this.flightpaths[fid][2]&&(this.flightpaths[fid][2].length>0)) {
            this.render_spartan_path({
                pts_ar: this.flightpaths[fid][2],
                lines: false,
                color: '#ff3399',
                ptrad: 2
            });
        }

        this.render_gc({
            pts_ar: this.flightpaths[fid][0],
            lines: false,
            color: '#33ff99',
            ptrad: 2
        });

    };

    this.render_single_path = function(cobj) {
        var ctx = this.canvasctxs.selpaths;

        var pts_ar=cobj.pts_ar;

        var ind;
        var thispt;
        var thisx,thisy,lastx,lasty;
        var startind=0;

        //pts_ar.sort(function(a,b) { return a.tss-b.tss; });

        if (cobj.lines) {
            lastx=this.x2chart(pts_ar[0].lon);
            lasty=this.y2chart(pts_ar[0].lat);
            startind=1;
        }

        for (ind=startind;ind<pts_ar.length;ind++) {
            thispt=pts_ar[ind];

            thisx=this.x2chart(thispt.lon);
            thisy=this.y2chart(thispt.lat);

            ctx.globalAlpha= cobj.alpha || 0.9;
            ctx.lineWidth = cobj.linewidth || 2;
            ctx.strokeStyle = cobj.color || '#c60fab';
            ctx.fillStyle = ctx.strokeStyle;


            if (cobj.lines) {
                ctx.beginPath();
                ctx.moveTo(lastx, lasty);
                ctx.lineTo(thisx, thisy);
                ctx.stroke();

                lastx=thisx;
                lasty=thisy;
            } else {
                ctx.beginPath();
                ctx.arc(thisx, thisy, cobj.ptrad || 2, 0, 2 * Math.PI, false);
                ctx.fill();
                ctx.stroke();
            }
        }

    };

    this.render_spartan_path = function(cobj) {
        var ctx = this.canvasctxs.selpaths;

        var pts_ar=cobj.pts_ar;

        var ind;
        var thispt;
        var thisx,thisy,lastx,lasty;
        var startind=0;

        //pts_ar.sort(function(a,b) { return a.tss-b.tss; });

        if (cobj.lines) {
            lastx=this.x2chart(pts_ar[0][0]);
            lasty=this.y2chart(pts_ar[0][1]);
            startind=1;
        }

        for (ind=startind;ind<pts_ar.length;ind++) {
            thispt=pts_ar[ind];

            thisx=this.x2chart(thispt[0]);
            thisy=this.y2chart(thispt[1]);

            ctx.globalAlpha= cobj.alpha || 0.9;
            ctx.lineWidth = cobj.linewidth || 2;
            ctx.strokeStyle = cobj.color || '#c60fab';
            ctx.fillStyle = ctx.strokeStyle;


            if (cobj.lines) {
                ctx.beginPath();
                ctx.moveTo(lastx, lasty);
                ctx.lineTo(thisx, thisy);
                ctx.stroke();

                lastx=thisx;
                lasty=thisy;
            } else {
                ctx.beginPath();
                ctx.arc(thisx, thisy, cobj.ptrad || 2, 0, 2 * Math.PI, false);
                ctx.fill();
                ctx.stroke();
            }
        }

    };

    this.render_gc = function(cobj) {
        var ctx = this.canvasctxs.selpaths;

        var pts_ar=cobj.pts_ar;

        var startxlon=pts_ar[0][0];
        var startylat=pts_ar[0][1];
        var endxlon=pts_ar[pts_ar.length-1][0];
        var endylat=pts_ar[pts_ar.length-1][1];

        var ind;
        var thispt;
        var thisx,thisy,lastx,lasty;
        var startind=0;
        var ll=pts_ar.length;

        //pts_ar.sort(function(a,b) { return a.tss-b.tss; });

        if (cobj.lines) {
            lastx=this.x2chart(startxlon);
            lasty=this.y2chart(startylat);
            startind=1;
        }

        var d=this.haversine(startxlon,startylat,endxlon,endylat);

        for (ind=startind;ind<pts_ar.length;ind++) {
            thispt=this.gcinterp(startxlon,startylat,endxlon,endylat,d,ind/ll);

            thisx=this.x2chart(thispt.xlon);
            thisy=this.y2chart(thispt.ylat);

            ctx.globalAlpha= cobj.alpha || 0.9;
            ctx.lineWidth = cobj.linewidth || 2;
            ctx.strokeStyle = cobj.color || '#c60fab';
            ctx.fillStyle = ctx.strokeStyle;


            if (cobj.lines) {
                ctx.beginPath();
                ctx.moveTo(lastx, lasty);
                ctx.lineTo(thisx, thisy);
                ctx.stroke();

                lastx=thisx;
                lasty=thisy;
            } else {
                ctx.beginPath();
                ctx.arc(thisx, thisy, cobj.ptrad || 2, 0, 2 * Math.PI, false);
                ctx.fill();
                ctx.stroke();
            }
        }

    };
}
