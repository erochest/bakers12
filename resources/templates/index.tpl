<!doctype html>
<!-- paulirish.com/2008/conditional-stylesheets-vs-css-hacks-answer-neither/ -->
<!--[if lt IE 7]> <html class="no-js ie6 oldie" lang="en"> <![endif]-->
<!--[if IE 7]>    <html class="no-js ie7 oldie" lang="en"> <![endif]-->
<!--[if IE 8]>    <html class="no-js ie8 oldie" lang="en"> <![endif]-->
<!-- Consider adding an manifest.appcache: h5bp.com/d/Offline -->
<!--[if gt IE 8]><!--> <html class="no-js" lang="en"> <!--<![endif]-->
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">

    <title>Bakers12</title>

    <meta name="description" content="">
    <meta name="author" content="">
    <meta name="viewport" content="width=device-width,initial-scale=1">

    <link href="/stylesheets/screen.css" media="screen, projection" rel="stylesheet" type="text/css" />
    <link href="/stylesheets/print.css" media="print" rel="stylesheet" type="text/css" />
    <!--[if lt IE 8]>
        <link href="/stylesheets/ie.css" media="screen, projection" rel="stylesheet" type="text/css" />
    <![endif]-->
    <script src="/js/libs/modernizr-2.0.6.min.js"></script>
  </head>
  <body class='bp'>
    <div id="container">
      <header>

        <h1>Bakers12</h1>

      </header>

      <div id="main" role="main" class='container'>
        <p>
         This is a simple interface to the Baker12 command-line script.
         Currently, you can only load a file. It's tokenized and the results are
         printed out. The running change of the <a
         href="http://en.wikipedia.org/wiki/Type-Token_Ratio">type-to-token
         ratio</a> is also created.
        </p>

        <div style="width: 100px">
          <form action="/tokenize/" method="post" enctype="multipart/form-data" class='bp'>
	    <fieldset>
	      <div class='clearfix'>
                <label for="file">File</label>
	        <div class='input'><input class='input' type="file" name="file" size="40" /></div>
              </div>
              <div class='actions'>
                <input type="submit" class='btn' value='Upload' />
              </div>
	    </fieldset>
          </form>
        </div>
      </div>

      <footer>

      </footer>
    </div> <!--! end of #container -->

    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.6.3/jquery.min.js"></script>
    <script>window.jQuery || document.write('<script src="/js/libs/jquery-1.6.3.min.js"><\/script>')</script>


    <script defer='defer' src="/js/plugins.js"></script>
    <script defer='defer' src='/js/libs/flot/jquery.flot.min.js'></script>
    <script defer='defer' src="/js/script.js"></script>


  <!-- Prompt IE 6 users to install Chrome Frame. Remove this if you want to support IE 6.
       chromium.org/developers/how-tos/chrome-frame-getting-started -->
  <!--[if lt IE 7 ]>
    <script src="//ajax.googleapis.com/ajax/libs/chrome-frame/1.0.3/CFInstall.min.js"></script>
    <script>window.attachEvent('onload',function(){CFInstall.check({mode:'overlay'})})</script>
  <![endif]-->
  
  </body>
</html>
