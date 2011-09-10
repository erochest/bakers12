<html>
  <head>
    <title>Bakers12</title>
    <link rel="stylesheet" type="text/css" href="screen.css"/>
  </head>
  <body>
    <div id="content">
      <h1>Bakers12</h1>
      <p>
       This is a simple interface to the Baker12 command-line script.
       Currently, you can only load a file. It's tokenized and the results are
       printed out. The running change of the <a
       href="http://en.wikipedia.org/wiki/Type-Token_Ratio">type-to-token
       ratio</a> is also created.
      </p>

      <div style="width: 100px">
        <form action="/tokenize/" method="post" enctype="multipart/form-data">
          <label for="file">File</label> &nbsp; <input type="file" name="file" size="40" /><br />
          <div style="text-align: right;">
            <input type="submit" />
          </div>
        </form>
      </div>
    </div>
  </body>
</html>
