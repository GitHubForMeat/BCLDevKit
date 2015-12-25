/* Change history:
 *
 * Date 	Who	What
 * ==========================================================================================
 * 10.10.2013	ASI	Updated to show mono in and out peak levels instead of
 *                      left and right input levels
 *                      Added zero value check before recalculating from linear to dB
 *
 */
 
// global variables
var old_parsed=null;

function startUpdate(ms)
{
	window.setInterval("reloadData()",ms);
}

function reloadData() {
	if (window.XMLHttpRequest)
	{// code for IE7+, Firefox, Chrome, Opera, Safari
		req=new XMLHttpRequest();
		// fix for MSIE
		if (req.overrideMimeType) req.overrideMimeType("text/html; charset=ISO-8859-1");
	}
	else
	{// code for IE6, IE5
		req=new ActiveXObject("Microsoft.XMLHTTP");
	}
	req.open("GET","status_data.html",true);
	// workaround for MSIE caching problem
	req.setRequestHeader("If-Modified-Since", "Sat, 1 Jan 2000 00:00:00 GMT");
	req.onreadystatechange=function() { if (this.readyState==4 && this.status==200) updateLoadedValues(this.responseText); }
	try { req.send(null);} catch(err) {}

  	return false;
}

function updateLoadedValues(raw_data){
	var parsed=raw_data.split(";");

	function chk(i)
	{
		return !old_parsed || old_parsed[i]!=parsed[i];
	}
	
	function lin2db(linear){
		//converts from linear scale (0-7FFF) to dB
		return parseInt(20 * (Math.log(linear/32767)/Math.log(10)),10);
	}
	

	if (chk(1)) {
		set_txt('volID',parsed[1]*5+" %");
		update_vu_meter(vuV,parsed[1]*5);
	}
	if (chk(2)) {
		if (parsed[2] == 0){parsed[2]=1;} // prevent calculating logarithm out of 0
		set_txt('peaklinID',(lin2db(parsed[2])<=-127?"-&infin;":lin2db(parsed[2]))+" dB");
		update_vu_meter(vuInL,lin2db(parsed[2]));
	}
	if (chk(3)) {
		if (parsed[3] == 0){parsed[3]=1;} // prevent calculating logarithm out of 0
		set_txt('peakloutID',(lin2db(parsed[3])<=-127?"-&infin;":lin2db(parsed[3]))+" dB");
		update_vu_meter(vuOutL,lin2db(parsed[3]));
	}
	if (chk(4) || chk(5)) {
		set_txt('rmthost',(parsed[4].length>2?parsed[4]+" : "+parsed[5]:"No Stream"));
	}

	old_parsed=parsed;
}
