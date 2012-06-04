<apply template="base">

      <p>
        This is a simple demo page served using
        <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
        and the <a href="http://snapframework.com/">Snap</a> web framework.
      </p>

      <ifLoggedIn><p>Congrats!  You're logged in as '<loggedInUser/>'</p></ifLoggedIn>

      <p><a href="/logout">Logout</a></p>

</apply>
