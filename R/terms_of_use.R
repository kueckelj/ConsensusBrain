html_terms_of_use <-
  shiny::tagList(
    shiny::tags$h4("1. Use of the App"),
    shiny::tags$p("You are granted a limited, non-exclusive, non-transferable, revocable license to access and use the App solely for the purpose of participating in the Study. You agree not to:"),
    shiny::tags$ul(
      shiny::tags$li("Share your login credentials or allow others to access the App using your account."),
      shiny::tags$li("Copy, distribute, modify, or create derivative works of the App’s content or design."),
      shiny::tags$li("Use the App in any unlawful or unauthorized manner.")
    ),

    shiny::tags$h4("2. Confidentiality Agreement"),
    shiny::tags$p("You acknowledge that in using the App, you may have access to confidential or proprietary information (“Confidential Information”), including but not limited to research goals, data structure, algorithmic models, and participant data. You agree to:"),
    shiny::tags$ul(
      shiny::tags$li("Maintain the confidentiality of all Confidential Information;"),
      shiny::tags$li("Not disclose Confidential Information to any third party;"),
      shiny::tags$li("Use the Confidential Information solely for the purpose of participating in the Study.")
    ),
    shiny::tags$p("Confidential Information does not include information that is (i) publicly known through no fault of yours, (ii) disclosed to you by a third party lawfully, or (iii) required to be disclosed by law or court order (with prior notice to Us, if legally permitted)."),

    shiny::tags$h4("3. Ownership and Copyright"),
    shiny::tags$p("All materials provided through the App, including but not limited to text, graphics, data, algorithms, software, and documentation (excluding User Input), are the sole property of Us or Our licensors and are protected by copyright, trademark, and other intellectual property laws."),
    shiny::tags$p("You acquire no ownership or rights in or to the App or its contents other than the limited license granted above."),

    shiny::tags$h4("4. Intellectual Property Assignment"),
    shiny::tags$p("By submitting any data, ratings, ideas, suggestions, feedback, comments, analysis, or any other intellectual contribution through or in connection with the App (collectively, the “Input”), You irrevocably assign to Us all rights, title, and interest in and to such Input, including but not limited to any intellectual property rights therein."),
    shiny::tags$p("This assignment includes the right for Us to use, reproduce, adapt, publish, translate, distribute, display, commercialize, and create derivative works from such Input, in whole or in part, in any form or media, whether now known or hereafter developed, and for any purpose, including commercial purposes, without compensation to You."),
    shiny::tags$p("You waive any moral rights or similar rights that may exist under applicable law."),

    shiny::tags$h4("5. Termination"),
    shiny::tags$p("We may suspend or terminate your access to the App at any time, with or without cause. Upon termination, all rights and obligations under this Agreement shall remain in effect with respect to past use, including intellectual property rights assigned to Us and confidentiality obligations."),

    shiny::tags$h4("6. Entire Agreement"),
    shiny::tags$p("This Agreement constitutes the entire agreement between You and Us regarding your use of the App and supersedes all prior or contemporaneous communications, whether electronic, oral, or written.")
  )
