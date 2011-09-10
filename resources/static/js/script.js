
jQuery(function() {
    jQuery.plot(jQuery('#ratiograph'), [
                {
                    data: window.ratioData, 
                    lines: { show: true }
                }
    ],
    { yaxis: { max: 1, min: 0 } });
});

