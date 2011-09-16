
jQuery(function() {
    jQuery.plot(jQuery('#ratiograph'), [
                {
                    data: window.ratioData, 
                    lines: { show: true }
                }
    ],
    { yaxis: { max: 1, min: 0 } });
    jQuery.plot(jQuery('#freqgraph'), [
                {
                    data: window.freqData, 
                    lines: { show: true }
                }
    ],
    { yaxis: { min: 0 } });
});

