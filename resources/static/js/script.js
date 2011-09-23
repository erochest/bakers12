(function() {
  $(function() {
    $('#ratiograph').each(function(idx, ratioGraph) {
      return $.plot(ratioGraph, [
        {
          data: window.ratioData,
          lines: {
            show: true
          }
        }
      ], {
        yaxis: {
          max: 1,
          min: 0
        }
      });
    });
    return $('#freqgraph').each(function(idx, freqGraph) {
      return $.plot(freqGraph, [
        {
          data: window.freqData,
          lines: {
            show: true
          }
        }
      ], {
        yaxis: {
          min: 0
        }
      });
    });
  });
}).call(this);
