<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet" type="text/css" href="screen.css"/>
  </head>
  <body>
    <div id="content">
      <h1>It works!</h1>
      <p>
        This is a simple demo page served using
        <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
        and the <a href="http://snapframework.com/">Snap</a> web framework.
      </p>
      <p>
        Echo test:
        <a href="/echo/cats">cats</a>
        <a href="/echo/dogs">dogs</a>
        <a href="/echo/fish">fish</a>
      </p>
      <table id="info">
        <tr>
          <td>Config generated at:</td>
          <td><start-time/></td>
        </tr>
        <tr>
          <td>Page generated at:</td>
          <td><current-time/></td>
        </tr>
      </table>
    </div>
  </body>
</html>
