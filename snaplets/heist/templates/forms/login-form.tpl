<apply template="default">

  <dfForm id="login-form" class="well">

    <h2>User &rarr; Login</h2>

    <hr />

    <dfChildErrorList class="alert alert-error" ref="" />

    <dfLabel class="control-label" ref="username">Username</dfLabel>
    <dfInputText ref="username" />

    <dfLabel class="control-label" ref="password">Password</dfLabel>
    <dfInputPassword ref="password" />

    <dfLabel class="control-label" ref="remember">Remember me</dfLabel>
    <dfInputCheckbox ref="remember" />

    <br />

    <dfInputSubmit class="btn btn-primary" value="Login" />

    <p style="text-align: center">
      <a href="/register/">or create a new account</a>
    </p>

  </dfForm>

</apply>
