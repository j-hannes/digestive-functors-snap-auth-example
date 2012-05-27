<apply template="default">

  <div class="row">
    <div class="span4 offset4">

      <dfForm id="student-login-form" class="well">

        <h2>Student &rarr; Login</h2>
        <hr />

        <dfChildErrorList class="alert alert-error" ref="" />

        <dfLabel class="control-label" ref="school">Schule</dfLabel>
        <dfInputSelect ref="school" />
        <br />

        <dfLabel class="control-label"
                 ref="studentId">
          Matrikelnummer
        </dfLabel>
        <dfInputText ref="studentId" placeholder="12345" />
        <br />

        <dfLabel class="control-label" ref="password">Passwort</dfLabel>
        <dfInputPassword ref="password" />
        <br />

        <dfInputSubmit class="btn btn-primary" value="Login" />

      </dfForm>

      <p style="text-align: center">
        <a href="/student/register/">neuen Account anlegen</a>
      </p>

    </div>
  </div>

</apply>
