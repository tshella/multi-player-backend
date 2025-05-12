{{- define "game-backend.name" -}}
game-backend
{{- end }}

{{- define "game-backend.fullname" -}}
{{ .Release.Name }}-game-backend
{{- end }}
