# 🎵 Sistema LastFM

O projeto consiste no desenvolvimento de um sistema de histórico musical, inspirado no funcionamento do **Last.fm**.

O sistema tem como objetivo registrar, analisar e apresentar o histórico musical de usuários, permitindo a análise de preferências musicais com base em contabilização de *scrobbles* — ou seja, cada execução de uma música registrada.

Todos os dados, como informações de usuários, execuções (*scrobbles*), estatísticas e catálogo musical, são armazenados de forma **persistente**, garantindo que o histórico e as preferências sejam mantidos entre diferentes sessões de uso.

---

## 👥 Autores
- Camila Rezende
- José Daniel Brandão
- Lorena Agra
- Marina Morais

---

## ✅ Funcionalidades

- 📌 Cadastro de usuários e login  
- 🎧 Registro de scrobbles  
- 🕓 Histórico do usuário  
- 🏆 Ranking pessoal e global  
- 🌎 Estatísticas globais
- 🥇 Desbloqueio de conquistas  
- 🎯 Recomendação personalizada  
- 💞 Compatibilidade entre perfis  

---

## 🚀 Como executar Haskell (via Cabal)

> Certifique-se de ter o [GHC](https://www.haskell.org/ghc/) e o [Cabal](https://www.haskell.org/cabal/) instalados.

# 1. Atualize os pacotes do cabal
cabal update

# 2. Instale as dependências
cabal build

# 3. Execute o projeto
cabal run

## 🚀 Como executar Prolog (via SWI-Prolog)

> Certifique-se de ter o [SWI-Prolog](https://www.swi-prolog.org/Download.html) instalado.

# 1. Navegue até o diretório 'main'
cd lastfm.plp\prolog\main

# 2. Inicie o inerpretador no SWI-Prolog
swipl

# 3. Execute o projeto
? - [main].

---

## 💾 Persistência
Os dados são armazenados nos seguintes arquivos:

- usuarios.json — usuários cadastrados
- scrobbles.json — execuções registradas
- catalogo.json — músicas disponíveis
