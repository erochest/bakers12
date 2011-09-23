
$ ->
	$('#ratiograph').each((idx, ratioGraph) ->
		$.plot(ratioGraph,
			[ { data: window.ratioData, lines: { show: true } } ],
			{ yaxis: { max: 1, min: 0 } })
	)
	$('#freqgraph').each((idx, freqGraph) ->
		$.plot(freqGraph,
			[ { data: window.freqData, lines: { show: true } } ],
			{ yaxis: { min: 0 } })
	)
