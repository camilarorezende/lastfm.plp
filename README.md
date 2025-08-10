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
- 🥇 Desbloqueio de conquistas  
- 🎯 Recomendação personalizada  
- 💞 Compatibilidade entre perfis  

---

## 🚀 Como executar (via Cabal)

> Certifique-se de ter o [GHC](https://www.haskell.org/ghc/) e o [Cabal](https://www.haskell.org/cabal/) instalados.

1. Atualize os pacotes do cabal
cabal update

2. Instale as dependências
cabal build

3. Execute o projeto
cabal run

---

## 💾 Persistência
Os dados são armazenados nos seguintes arquivos:

- usuarios.json — usuários cadastrados
- scrobbles.json — execuções registradas
- catalogo.json — músicas disponíveis
