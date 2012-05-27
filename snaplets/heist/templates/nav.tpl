<div class="navbar navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">

      <a class="brand" href="/">an example</a>

       <ul class="nav pull-right">
        <ifLoggedIn>
          <li>
            <a href="/logout">Logout</a>
          </li>
        </ifLoggedIn>
        <ifLoggedOut>
          <li>
            <a href="/login">Login</a>
          </li>
        </ifLoggedOut>
      </ul>

    </div>
  </div>
</div>
