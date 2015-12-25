/* name is the prefix given to the table cells representing the VU meter
 * discrete: <table class="vu_meter" id="name"><tr><td><td><td>...</table>
 * smooth: <table class="vu_meter" id="name"><tr><td><td></table>
 */
function update_vu_meter(vu,val)
{
	function set_element(obj,color,val)
	{
		obj.style.backgroundColor=val?color:"lightgrey";
	}
	var points=new Array(-96,-84,-72, -61,-45,-39,-33, -27,-24,-21,-18,-15,-12,-9,-6,-3);

	tbl=document.getElementById(vu.name);
	if (!vu.n) {
		tbl.style.borderSpacing="0px";
		tbl.style.padding="1px";
		row=tbl.rows[0];
		width=tbl.scrollWidth;
		if (val<vu.min) val=vu.min;
		if (val>vu.max) val=vu.max;
		filled=(val-vu.min)/(vu.max-vu.min);
		row.cells[0].style.backgroundColor=filled?vu.color:"lightgrey";
		row.cells[0].style.width=width*filled;
		row.cells[1].style.backgroundColor=filled>=1?vu.color:"lightgrey";
		row.cells[1].style.width=width*(1-filled);
	} else {
		for (i=0;i<vu.n;i++)
		{
			if (vu.color) color=vu.color;
			else {
				color="lime";
				if (points[i]>=-12) color="gold";
				if (points[i]>=-3) color="red";
			}
			if (vu.min!=vu.max) set=val>((i-vu.min)*(vu.max-vu.min)/vu.n);
			else set=(val>=points[i]);
			set_element(tbl.rows[0].cells[i],color,set);
		}
	}
}

/* optional:
 * 	color - not NULL use 1 color for all, otherwise VU green-yellow-red
 * 	n - if 0 then smooth meter, otherwise discrete with N steps
 * 	min,max - if not equal then linear interpolation, otherwise VU steps
 */
function VuMeter(name,cells,color,min,max)
{
	this.name=name;
	this.n=cells;
	this.color=color;
	this.min=min;
	this.max=max;
}
