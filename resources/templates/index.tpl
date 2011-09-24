<apply template='wrap'>
  <bind tag='pagetitle'>Bakers12</bind>
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
</apply>
