<apply template="default">

  <div class="row">
    <div class="span4 offset4">

      <dfForm id="student-registration-form" class="well">
      <!--<dfForm id="student-login-form" class="well">-->

        <h2>Account anlegen</h2>
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

        <dfLabel class="control-label" ref="firstName">Vorname</dfLabel>
        <dfInputText ref="firstName" />
        <br />

        <dfLabel class="control-label" ref="lastName">Nachname</dfLabel>
        <dfInputText ref="lastName" />
        <br />

        <dfLabel class="control-label" ref="email">E-Mail</dfLabel>
        <dfInputText ref="email" />
        <br />

        <dfInputSubmit class="btn btn-primary" value="Absenden" />

      </dfForm>

    </div>
  </div>

</apply>
