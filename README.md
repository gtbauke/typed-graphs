# Explorando as capacidades de tipos de Haskell para criação de grafos e suas operações

Haskell é uma linguagem de paradigma funcional que possui uma grande capacidade de manipulação de tipos, sendo possível realizar diversas operações no nível de tipos em tempo de compilação. Essa capacidade da linguagem pode ser muito útil para criação de estruturas de dados seguras em tempo de compilação. Neste projeto, exploramos as capacidades do Haskell na criação de grafos com diversas propriedades e operações.

Este projeto necessita de algumas extensões da linguagem Haskell. São elas:

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
```

## Representando grafos

Antes de iniciarmos a implementar as operações em grafos, precisamos definir como iremos representar essas estruturas no nosso programa. Podemos representar um grafo de diversas formas, como por exemplo, através de uma lista de adjacências, uma matriz de adjacências, uma sequência de arestas, uma sequências de nós, entre outras. Neste projeto, optamos por representar os grafos por meio de uma lista de adjacências, ou seja, uma lista que contém todas as arestas de um grafo.

Inicialmente, vamos representar apenas arestas direcionadas que possuem um peso associado. Para isso, usaremos o seguinte GADT:

```hs
type Edge :: Symbol -> Symbol -> Nat -> Type
data Edge from to weight where
  Edge :: Edge from to 0
  WeightedEdge :: Edge from to weight

deriving instance Show (Edge from to weight)
```

Aqui já notamos uma grande diferença na representação padrão de grafos. Todas as informações da aresta, como nó de saída, nó de chegada e peso estão sendo representadas no mundo dos tipos, invés de serem representadas no mundo dos valores. Isso é o primeiro passo para criarmos operações seguras no nível dos tipos.

Toda aresta na nossa representação possui três propriedades: o nó de saída, o nó de chegada e o peso. As duas primeiras são representadas por meio de símbolos, enquanto o peso é representado por um número natural. A aresta `Edge` representa uma aresta sem peso, enquanto a `WeightedEdge` representa uma aresta com peso.

Se tentarmos imprimir uma aresta na tela, veremos que independente das propriedades da aresta, sempre teremos o mesmo resultado:

```hs
Edge -- para uma aresta sem peso
WeightedEdge -- para uma aresta com peso
```

Isso acontece porque nossa representação da aresta vive no mundo dos tipos. Se quisermos imprimir suas propriedades na tela, precisamos de uma forma de converter tipos em valores. Para isso, usaremos um tipo muito especial chamado `Proxy`, que é um tipo que carrega consigo informações sobre um tipo, mas que não carrega valores. Com ele, podemos criar uma instância de `Show` para nossa aresta:

```hs
import Data.Proxy
import GHC.TypeLits

instance (KnownSymbol from, KnownSymbol to, KnownNat weight) => Show (Edge from to weight) where
  show _ = fromStr ++ " -" ++ wStr ++ "-> " ++ toStr
    where
      w = natVal (Proxy @weight)
      wStr = if w /= 0 then show w else ""

      fromStr = symbolVal (Proxy @from)
      toStr = symbolVal (Proxy @to)
```

A forma pela qual conseguimos transformar um tipo em um valor é usando as funções `natVal` e `symbolVal` do módulo `GHC.TypeLits`. Essas funções recebem um `Proxy t` e retornam o valor associado ao tipo `t`, desde que eles obedeçam a restrição de `KnownNat` para números naturais e `KnownSymbol` para símbolos.

Para especificar o tipo da `Proxy` que estamos operando, usamos a notação `@t`, onde `t` é o tipo que queremos especificar. Para isso, precisamos utilizar a extensão `TypeApplications`. Veremos que essa notação será importante mais para frente quando estivermos construindo nossos grafos.

Agora que podemos representar uma aresta em específico, podemos criar uma lista de arestas que representam um grafo:

```hs
edges' = [
  Edge @"a" @"b",
  Edge @"b" @"c",
  Edge @"c" @"a"
  ]
```

Apesar de parecer que está tudo certo, se tentarmos compilar nosso código teremos um erro. Isso acontece porque o GHC não consegue inferir o tipo da lista de arestas. Cada aresta tem um tipo diferente, então não existe um tipo único que represente todas as arestas. Para resolver esse problema, vamos criar um tipo novo que tenha a capacidade de guardar elementos dos mais diferentes tipos. Vamos chamar esse tipo de `HList`:

```hs
data HList xs where
  Nil :: HList '[]
  (:#) :: x -> HList xs -> HList (x ': xs)

infixr 5 :#
```

<!-- TODO: explain how HList works -->
<!-- TODO: explain restrictions in printing HList -->
<!-- TODO: explain type families -->
<!-- TODO: create more complex type families that enforce graph properties -->
<!-- TODO: start with ensuring graph does not contain cycles -->
