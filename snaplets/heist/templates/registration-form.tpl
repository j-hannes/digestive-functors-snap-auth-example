<apply template="default">

  <div class="row">
    <div class="span4 offset4">

      <dfForm id="registration-form" class="well">

        <h2>New account</h2>
        <hr />

        <dfChildErrorList class="alert alert-error" ref="" />

        <dfLabel class="control-label" ref="username">Username</dfLabel>
        <dfInputText ref="username" />

        <dfLabel class="control-label" ref="firstname">First name</dfLabel>
        <dfInputText ref="firstname" />

        <dfLabel class="control-label" ref="lastname">Last name</dfLabel>
        <dfInputText ref="lastname" />

        <dfLabel class="control-label" ref="email">Email</dfLabel>
        <dfInputText ref="email" />

        <br />

        <dfInputSubmit class="btn btn-primary" value="Send" />

      </dfForm>

    </div>
  </div>

</apply>
