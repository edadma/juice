Overview
========

this is an overview

[= warning =]
Using absolute paths with links is not officially supported. Relative paths are adjusted by MkDocs to ensure they are always relative to the page. Absolute paths are not modified at all. This means that your links using absolute paths might work fine in your local environment but they might break once you deploy them to your production server.
[= /warning =]

```squiggly
{{ define main }}
  <section class="section">
    <div class="columns">
      <div class="column is-2 is-offset-1">
        <aside class="menu">
          {{- for .site.toc }}
            {{- if .label }}
              <p class="menu-label">
                {{ .label }}
              </p>
            {{- else }}
              <ul class="menu-list">
                {{- for .headings }}
                  <li><a href="{{ .href | relURL }}">{{ .html }}</a></li>
                {{- end }}
              </ul>
            {{- end }}
          {{- end }}
        </aside>
      </div>

      <div class="column is-5">
        <div class="content has-text-justified">
          {{ .content }}
        </div>
      </div>

      <div class="column is-one-fifth">
        <aside class="menu">
          {{- if .sub }}
            <p class="menu-label has-text-weight-bold">
              Table of Contents
            </p>
            {{- partial 'subheadings' .sub }}
          {{- end }}
        </aside>
      </div>
    </div>
  </section>
{{ end }}
```

[= button 'Get Started &rarr;' html/general/overview /=]
