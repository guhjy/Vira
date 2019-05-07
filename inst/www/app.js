$(function(){
	//Handler for basic RPC
	$("#scorebutton").click(function(e){
		e.preventDefault()
		$(".riskfield").val("")
		$(".clusterfield").val("")
		var data = [];
		$("tbody tr").each(function(i){
			data[i] = {
			  id : parseInt($(this).find(".idfield").val()),
				age : parseFloat($(this).find(".agefield").val()),
				sex : $(this).find(".sexfield").val(),
				time : parseFloat($(this).find(".timefield").val()),
				acei : $(this).find(".aceifield").val(),
				dm : $(this).find(".dmfield").val(),
				creat : parseFloat($(this).find(".creatfield").val()),
				hc : $(this).find(".hcfield").val(),
				prenyha : $(this).find(".prenyhafield").val(),
				bsa : parseFloat($(this).find(".bsafield").val())
			};
		});
		
		//RPC request to score new data
		var req = ocpu.rpc("predRiskScore", {input : data}, function(output){
			//repopulate the table
			$("tbody tr").each(function(i){
			  $(this).find(".idfield").val(output[i].id);
				$(this).find(".agefield").val(output[i].age);
				$(this).find(".sexfield").val(output[i].sex);
				$(this).find(".timefield").val(output[i].time);
				$(this).find(".aceifield").val(output[i].acei);
				$(this).find(".dmfield").val(output[i].dm);
				$(this).find(".creatfield").val(output[i].creat);
				$(this).find(".hcfield").val(output[i].hc);
				$(this).find(".prenyhafield").val(output[i].prenyha);
				$(this).find(".bsafield").val(output[i].bsa);
				$(this).find(".riskfield").val(output[i].risk);
				$(this).find(".clusterfield").val(output[i].cluster);
			});
		}).fail(function(){
			alert(req.responseText);
		});
	});

	//CSV file scoring
	$("#csvfile").on("change", function loadfile(e){
		if(!$("#csvfile").val()) return;
		$("#outputcsv").addClass("hide").attr("href", "");
		$(".spinner").show()
		var req = ocpu.call("predRiskScore", {
			input : $("#csvfile")[0].files[0]
		}, function(tmp){
			$("#outputcsv").removeClass("hide").attr("href", tmp.getLoc() + "R/.val/csv")
		}).fail(function(){
			alert(req.responseText)
		}).always(function(){
			$(".spinner").hide()
		});
	});

	//update the example curl line with the current server
	$("#curlcode").text(
		$("#curlcode").text().replace(
			"https://localhost:5656/ocpu/apps/nguforche/Vira/R/predRiskScore/json", 
			window.location.href.match(".*/Vira/")[0] + "R/predRiskScore/json"
		)
	);

	//this is just to create a table
	function addrow(){
		$("tbody").append(
	'<tr> <td> <div class="form-group"> <input type="number" min="1" max="256" class="form-control idfield" placeholder="PID"></div> </td>'+
	'<td> <div class="form-group"> <input type="number" min="20" max="80" class="form-control agefield" placeholder="Age"></div> </td>' + 
	'<td> <div class="form-group"> <select class="form-control sexfield"> <option>Male</option> <option>Female</option></select> </div> </td>' +  
	'<td> <div class="form-group"> <input type="number" min="0" max="12" class="form-control timefield" placeholder="Time"></div> </td>' + 
	'<td> <div class="form-group"> <select class="form-control aceifield"> <option>Yes</option> <option>No</option></select></div> </td>' + 
	'<td> <div class="form-group"> <select class="form-control dmfield"> <option>Yes</option> <option>No</option></select></div> </td>' + 
	'<td> <div class="form-group"> <input type="number" min="50" max="700" class="form-control creatfield" placeholder="Creatinine"></div> </td>' + 
	'<td> <div class="form-group"> <select class="form-control hcfield"> <option>Absent</option> <option>Treated</option><option>Untreated</option></select></div> </td>' + 
	'<td> <div class="form-group"> <select class="form-control prenyhafield"> <option>I-II</option> <option>III-IV</option></select></div> </td>' + 
	'<td> <div class="form-group"> <input type="number" min="1" max="3" class="form-control bsafield" placeholder="BSA"></div> </td>' + 
	'<td> <div class="form-group"> <input disabled="disabled" class="disabled form-control riskfield"> </div> </td>' + 	
	'<td> <div class="form-group"> <input disabled="disabled" class="disabled form-control clusterfield"> </div> </td> </tr>');
	}

	for(var i = 0; i < 4; i++){
		addrow();
	}
});

