package com.example.production.infrastructure.graphql.resolver;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.out.BomRepository;
import com.example.production.domain.exception.ItemNotFoundException;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;
import com.example.production.domain.model.item.Item;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * BOM GraphQL リゾルバ実装
 * 既存の BomRepository を Input Adapter として呼び出す
 */
@Controller
public class BomResolver {

    private final BomRepository bomRepository;
    private final ItemUseCase itemUseCase;

    public BomResolver(BomRepository bomRepository, ItemUseCase itemUseCase) {
        this.bomRepository = bomRepository;
        this.itemUseCase = itemUseCase;
    }

    /**
     * 部品展開（BOM ツリー）
     */
    @QueryMapping
    public BomNode bomTree(@Argument String itemCode) {
        try {
            Item item = itemUseCase.getItemByCode(itemCode);
            List<BomExplosion> explosions = bomRepository.explode(itemCode, BigDecimal.ONE);

            return buildBomTree(item, explosions);
        } catch (ItemNotFoundException e) {
            return null;
        }
    }

    /**
     * 使用先照会（逆展開）
     */
    @QueryMapping
    public List<WhereUsedResult> whereUsed(@Argument String itemCode) {
        List<Bom> usages = bomRepository.findByChildItemCode(itemCode);

        return usages.stream()
            .map(bom -> {
                String itemName = "";
                try {
                    Item item = itemUseCase.getItemByCode(bom.getParentItemCode());
                    itemName = item.getItemName();
                } catch (ItemNotFoundException e) {
                    itemName = bom.getParentItemCode();
                }
                return WhereUsedResult.builder()
                    .parentItemCode(bom.getParentItemCode())
                    .itemName(itemName)
                    .requiredQuantity(bom.getRequiredQuantity())
                    .level(1)
                    .build();
            })
            .toList();
    }

    /**
     * BOM 展開結果からツリー構造を構築
     */
    private BomNode buildBomTree(Item rootItem, List<BomExplosion> explosions) {
        // ルートノードを作成
        BomNode root = BomNode.builder()
            .itemCode(rootItem.getItemCode())
            .itemName(rootItem.getItemName())
            .requiredQuantity(BigDecimal.ONE)
            .level(0)
            .children(new ArrayList<>())
            .build();

        if (explosions.isEmpty()) {
            return root;
        }

        // レベル別にノードを管理
        Map<String, BomNode> nodeMap = new HashMap<>();
        nodeMap.put(rootItem.getItemCode(), root);

        // レベル順にソートして処理
        List<BomExplosion> sortedExplosions = explosions.stream()
            .sorted((a, b) -> Integer.compare(a.getLevel(), b.getLevel()))
            .toList();

        for (BomExplosion explosion : sortedExplosions) {
            String childItemName = "";
            try {
                Item childItem = itemUseCase.getItemByCode(explosion.getChildItemCode());
                childItemName = childItem.getItemName();
            } catch (ItemNotFoundException e) {
                childItemName = explosion.getChildItemCode();
            }

            BomNode childNode = BomNode.builder()
                .itemCode(explosion.getChildItemCode())
                .itemName(childItemName)
                .requiredQuantity(explosion.getTotalQuantity())
                .level(explosion.getLevel())
                .children(new ArrayList<>())
                .build();

            nodeMap.put(explosion.getChildItemCode(), childNode);

            // 親ノードに追加
            BomNode parentNode = nodeMap.get(explosion.getParentItemCode());
            if (parentNode != null) {
                parentNode.getChildren().add(childNode);
            }
        }

        return root;
    }
}
